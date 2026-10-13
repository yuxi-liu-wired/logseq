(ns frontend.handler.copy-paste-fuzz-test
  "Metamorphic test of copy and paste in the app's own clipboard format: the
  root blocks of a page copied by `editor/copy-selection-blocks` (which puts
  the blocks on the clipboard as the `web application/logseq` type), read back
  by `paste/paste-copied-blocks-or-text` and inserted by `editor/paste-blocks`
  after a block of another page, must be the copied blocks: tree shape, title
  (refs shown as titles), heading, status, priority, display type, embed
  (:block/link), collapsed (blocks with children), tags, user properties.
  The worker is answered in process (test db), outliner ops are applied in
  process with the worker's transact pipeline, so pasted blocks are what the
  app stores. One small page per kind and seed, so a mismatch belongs to one
  kind and its example is small; the kind :mixed combines all. Mismatches are
  grouped by kind and mismatch, each with its smallest example.
  Env: FUZZ_N seeds per kind (default 10), FUZZ_SEED first seed (default 1),
  FUZZ_KINDS comma-separated subset (default all)."
  (:require [cljs.test :refer [async deftest is use-fixtures]]
            [clojure.string :as string]
            [datascript.core :as d]
            [datascript.impl.entity :as de]
            [frontend.db.conn :as conn]
            [frontend.db.transact :as db-transact]
            [frontend.handler.editor :as editor]
            [frontend.handler.export.html :as export-html]
            [frontend.handler.paste :as paste-handler]
            [frontend.state :as state]
            [frontend.test.helper :as test-helper]
            [frontend.util :as util]
            [frontend.worker.export :as worker-export]
            [frontend.worker.handler.block :as worker-block-handler]
            [logseq.common.util :as common-util]
            [logseq.db.frontend.content :as db-content]
            [logseq.db.frontend.property :as db-property]
            [logseq.db.sqlite.build :as sqlite-build]
            [logseq.outliner.op :as outliner-op]
            [promesa.core :as p]))

(defn- env [k] (aget js/process.env k))
(defn- env-int [k default]
  (or (some-> (env k) js/parseInt) default))
(defn- env-keywords [k default]
  (if-let [s (not-empty (env k))]
    (filterv (set (map keyword (string/split s #","))) default)
    default))

(def fuzz-n (env-int "FUZZ_N" 10))
(def fuzz-seed (env-int "FUZZ_SEED" 1))

;; ---------------------------------------------------------------------------
;; seeded random

(defn- rng [seed] (atom (max 1 (mod (* 48271 (inc seed)) 2147483647))))
(defn- next! [r n]
  (swap! r #(mod (* 48271 %) 2147483647))
  (mod @r n))
(defn- pick [r coll] (nth coll (next! r (count coll))))
(defn- chance [r pct] (< (next! r 100) pct))
(defn- some-of [r coll] (vec (distinct (repeatedly (inc (next! r (count coll))) #(pick r coll)))))
(defn- rng-uuid [r]
  (let [h #(.padStart (.toString (next! r 65536) 16) 4 "0")]
    (uuid (str (h) (h) "-" (h) "-4" (subs (h) 1) "-8" (subs (h) 1) "-" (h) (h) (h)))))

;; ---------------------------------------------------------------------------
;; inputs (the generator of markdown-mirror-import-fuzz-test and export-import-fuzz-test)

(def ^:private ref-pages ["Alpha" "Beta Gamma" "日本語" "ns/child"])
(def ^:private journal-days [20240101 20251231])
(def ^:private classes [:research :idea])
(def ^:private words ["alpha" "Beta" "x" "中文" "ünïcödé" "émoji😀" "42" "3.14" "a:b" "tail."])

(def ^:private specials
  ["#" "## " "[[Alpha]]" "[[Beta Gamma]]" "#[[Beta Gamma]]" "#research" "key:: v" "::" "**bold**" "*em*"
   "`code`" "$$x$$" "> " "- " "1. " "TODO " "DONE " "LATER " "http://example.com/a?b=c" "\"q\"" "'"
   "\\" "  " "\t" "{{embed}}" "((nope))" "<b>" "&amp;" "~~s~~" "^^h^^" "==m==" "[x](http://y.z)"
   "id:: 6aa" "collapsed:: true" "|a|b|" "---" "%" "$" "@" "!" "?" "\n"
   "```clojure\n(x 1)\n```" "|a|b|\n|-|-|\n|1|2|" "#+BEGIN_QUOTE\nq\n#+END_QUOTE" "SCHEDULED: <2024-01-01 Mon>"
   "DEADLINE: <2025-12-31 Wed>" "[label]([[Alpha]])" "#[[ns/child]]" "[[ns/child]]" "\\(x\\)" "[^1]" "<!-- c -->"
   "* " "+ " ":PROPERTIES:\n:x: 1\n:END:" "[#A] " "{{query (task TODO)}}" "{{embed [[Alpha]]}}" "![img](../assets/x.png)"
   "\n  - " "\n\n" "&" "<" ">" ":tag/x" "{:a 1}" "#{}" "[:block/uuid x]" "\"" "\\n"])

(def ^:private chat-titles
  "Inputs tried in chat, kept verbatim."
  ["^^[omega]^^" "^^ω^^" "(=^･ω･^=)" "ฅ^•ﻌ•^ฅ" "OwO"])

(defn- word-title [r] (pick r words))

(defn- special-title [r]
  (string/join (repeatedly (inc (next! r 5)) #(if (chance r 50) (pick r specials) (str (pick r words) " ")))))

(def ^:private choice-uuids
  {:p-choice [#uuid "00000000-0000-4000-8000-0000000000c1" #uuid "00000000-0000-4000-8000-0000000000c2"]
   :p-choices [#uuid "00000000-0000-4000-8000-0000000000c3" #uuid "00000000-0000-4000-8000-0000000000c4"]})

(def ^:private property-types
  {:p-text :default :p-texts :default :p-number :number :p-check :checkbox :p-url :url :p-date :date
   :p-node :node :p-choice :default :p-choices :default})

(def ^:private many-properties #{:p-texts :p-node :p-choices})

(defn- gen-value [r k]
  (case k
    :p-choice [:block/uuid (pick r (:p-choice choice-uuids))]
    :p-choices (set (map (fn [u] [:block/uuid u]) (some-of r (:p-choices choice-uuids))))
    :p-text (if (chance r 50) (word-title r) (special-title r))
    :p-texts (set (repeatedly (inc (next! r 3)) #(if (chance r 50) (word-title r) (special-title r))))
    :p-number (pick r [0 1 -2 3.5 1000000 0.1])
    :p-check (chance r 50)
    :p-url (pick r ["https://logseq.com" "http://example.com/a?b=c" "https://x.y/a(b)c"])
    :p-date [:build/page {:build/journal (pick r journal-days)}]
    :p-node (set (map (fn [t] [:build/page {:block/title t}]) (some-of r ref-pages)))))

(def ^:private kinds
  (into [:chat :title :heading :status :priority :tag :code :quote :math :block-ref :embed :link
         :collapsed :nesting :pvalue-block]
        (concat (keys property-types) [:mixed])))

(defn- block-for-kind
  "A block that exercises kind; seen holds the uuids of earlier blocks of the page."
  [r kind seen depth]
  (let [uuid (rng-uuid r)
        mixed? (= kind :mixed)
        on? (fn [k pct] (or (= kind k) (and mixed? (chance r pct))))
        code? (on? :code 8) math? (and (not code?) (on? :math 5)) quote? (and (not code?) (not math?) (on? :quote 5))
        link (when (and (seq @seen) (not code?) (not math?) (not quote?) (on? :link 10)) (pick r @seen))
        ref-token (fn [] (when (seq @seen)
                           (cond
                             (on? :block-ref 20) (str "[[" (pick r @seen) "]]")
                             (on? :embed 10) (str "{{embed [[" (pick r @seen) "]]}}"))))
        title (cond
                link ""
                code? (pick r ["(println \"hi\")\n(+ 1 2)" "SELECT 1;" "  indented\n\ttab" "- not a block\n# not a heading"])
                math? (pick r ["E = mc^2" "\\frac{a}{b}"])
                (= kind :chat) (pick r chat-titles)
                :else (str (if (on? :title 40) (special-title r) (word-title r))
                           (some->> (ref-token) (str " "))))
        pvalue-block (when (and (seq @seen) (on? :pvalue-block 10)) (pick r @seen))
        _ (swap! seen conj uuid)
        props (cond-> (into {} (for [k (keys property-types) :when (on? k 15)] [k (gen-value r k)]))
                pvalue-block (update :p-node (fnil conj #{}) [:block/uuid pvalue-block]))
        built-ins (cond-> {}
                    (and (not code?) (not math?) (not quote?) (on? :heading 15))
                    (assoc :logseq.property/heading (inc (next! r 6)))
                    (on? :status 15) (assoc :logseq.property/status
                                            (pick r [:logseq.property/status.todo :logseq.property/status.doing
                                                     :logseq.property/status.done]))
                    (on? :priority 8) (assoc :logseq.property/priority
                                             (pick r [:logseq.property/priority.high :logseq.property/priority.low]))
                    code? (assoc :logseq.property.node/display-type :code
                                 :logseq.property.code/lang (pick r ["clojure" "sql" "text"]))
                    quote? (assoc :logseq.property.node/display-type :quote)
                    math? (assoc :logseq.property.node/display-type :math))
        tags (cond-> (if (on? :tag 20) (some-of r classes) [])
               ;; as the app's set-property does (outliner.property/should-add-task-tag-for-property?)
               (:logseq.property/status built-ins) (conj :logseq.class/Task)
               code? (conj :logseq.class/Code-block)
               quote? (conj :logseq.class/Quote-block)
               math? (conj :logseq.class/Math-block))
        nest? (and (< depth 3) (or (and (= kind :nesting) (chance r 60)) (and mixed? (chance r 30))))]
    (cond-> {:block/title title
             :block/uuid uuid
             :build/keep-uuid? true}
      link (assoc :block/link [:block/uuid link])
      (or (seq props) (seq built-ins)) (assoc :build/properties (merge props built-ins))
      (seq tags) (assoc :build/tags tags)
      (on? :collapsed 10) (assoc :block/collapsed? true)
      nest? (assoc :build/children (vec (repeatedly (inc (next! r 3)) #(block-for-kind r kind seen (inc depth))))))))

(defn- gen-page [kind seed]
  (let [r (rng (+ seed (* 7919 (.indexOf kinds kind))))
        seen (atom [])]
    {:kind kind
     :seed seed
     :page {:block/title (str "fuzz " (name kind) " " seed)}
     :target-page (str "paste target " (name kind) " " seed)
     :blocks (vec (repeatedly (inc (next! r 3)) #(block-for-kind r kind seen 0)))}))

(def ^:private pages
  (vec (for [kind (env-keywords "FUZZ_KINDS" kinds) seed (range fuzz-seed (+ fuzz-seed fuzz-n))]
         (gen-page kind seed))))

;; ---------------------------------------------------------------------------
;; readable forms, the same for the copied and the pasted blocks

(defn- readable-value [v]
  (cond
    (set? v) (set (map readable-value v))
    (de/entity? v) (or (:block/journal-day v)
                       (when (:block/title v) (db-content/recur-replace-uuid-in-block-title v))
                       (db-property/property-value-content v))
    :else v))

(defn- ident-name [v]
  (cond (keyword? v) (name v) (map? v) (some-> (:db/ident v) name) (de/entity? v) (some-> (:db/ident v) name) :else v))

(defn- user-props [e]
  (into {} (for [[k v] (db-property/properties e)
                 :when (= "user.property" (namespace k))]
             [(name k) (readable-value v)])))

(defn- child-blocks [parent]
  (->> (:block/_parent parent)
       (remove :logseq.property/created-from-property)
       (sort-by :block/order)))

(defn- block-view [e depth]
  {:depth depth
   :title (db-content/recur-replace-uuid-in-block-title e)
   :heading (:logseq.property/heading e)
   :status (ident-name (:logseq.property/status e))
   :priority (ident-name (:logseq.property/priority e))
   :display-type (:logseq.property.node/display-type e)
   :link (some-> (:block/link e) db-content/recur-replace-uuid-in-block-title)
   :collapsed? (boolean (:block/collapsed? e))
   :children? (boolean (seq (child-blocks e)))
   :tags (set (map :block/title (:block/tags e)))
   :props (user-props e)})

(defn- tree-views [blocks depth]
  (mapcat (fn [b] (cons (block-view b depth) (tree-views (child-blocks b) (inc depth)))) blocks))

(defn- find-page [db title]
  (some->> (d/q '[:find ?p . :in $ ?t :where [?p :block/title ?t] [?p :block/name]] db title)
           (d/entity db)))

(defn- mismatch-kinds [src dst]
  (let [plain? (and (empty? (:props src)) (empty? (:tags src)) (nil? (:heading src)) (nil? (:status src))
                    (nil? (:priority src)) (nil? (:display-type src)))
        t-src (string/trim (str (:title src)))
        t-dst (string/trim (str (:title dst)))]
    (cond-> []
      (not= (:depth src) (:depth dst)) (conj :structure/depth)
      (and (not= (:title src) (:title dst)) (= t-src t-dst)) (conj :title/outer-whitespace)
      (and (not= t-src t-dst) plain?) (conj :title/plain-block)
      (and (not= t-src t-dst) (not plain?)) (conj :title/block-with-metadata)
      (not= (:heading src) (:heading dst)) (conj :heading)
      (not= (:status src) (:status dst)) (conj :status)
      (not= (:priority src) (:priority dst)) (conj :priority)
      (not= (:display-type src) (:display-type dst))
      (conj (keyword "display-type" (str (or (some-> (:display-type src) name) "none") "->"
                                         (or (some-> (:display-type dst) name) "none"))))
      (not= (:link src) (:link dst)) (conj :link)
      (and (:children? src) (not= (:collapsed? src) (:collapsed? dst))) (conj :collapsed)
      (not= (:tags src) (:tags dst)) (conj :tags)
      true (into (for [[k v] (:props src) :when (not= v (get (:props dst) k))]
                   (keyword "property" k)))
      true (into (for [k (keys (:props dst)) :when (not (contains? (:props src) k))]
                   :property/extra)))))

(defn- compare-views [src dst]
  (if (not= (count src) (count dst))
    [{:mismatch :structure/block-count :src (vec src) :dst (vec dst)}]
    (vec (for [[s t] (map vector src dst)
               m (mismatch-kinds s t)]
           {:mismatch m :src s :dst t}))))

;; ---------------------------------------------------------------------------
;; the loop

(use-fixtures :once
  {:before (fn []
             (test-helper/start-test-db!)
             (let [{:keys [init-tx block-props-tx]}
                   (sqlite-build/build-blocks-tx
                    {:pages-and-blocks (-> (mapv (fn [t] {:page {:block/title t}}) ref-pages)
                                           (into (map (fn [day] {:page {:build/journal day}}) journal-days))
                                           (into (map #(select-keys % [:page :blocks]) pages))
                                           (into (map (fn [{:keys [target-page]}]
                                                        {:page {:block/title target-page}
                                                         :blocks [{:block/title "Target"}]})
                                                      pages)))
                     :classes (into {} (map (fn [c] [c {}]) classes))
                     :properties
                     (into {} (for [[k t] property-types]
                                [k (cond-> {:logseq.property/type t}
                                     (many-properties k) (assoc :db/cardinality :db.cardinality/many)
                                     (= k :p-choice) (assoc :build/closed-values
                                                            (mapv (fn [v u] {:value v :uuid u}) ["joy" "sad"] (:p-choice choice-uuids)))
                                     (= k :p-choices) (assoc :build/closed-values
                                                             (mapv (fn [v u] {:value v :uuid u}) ["red" "blue"] (:p-choices choice-uuids))))]))})]
               (d/transact! (conn/get-db test-helper/test-db false)
                            (concat (map #(select-keys % [:block/uuid]) init-tx) init-tx block-props-tx))))
   :after test-helper/destroy-test-db!})

(defn- worker-stub
  "The db worker's answers for the calls copy and paste make, from the test db."
  [api repo & args]
  (let [db (conn/get-db repo)]
    (case api
      :thread-api/q
      (let [[inputs] args] (p/resolved (apply d/q (first inputs) db (rest inputs))))
      :thread-api/get-blocks
      (let [[requests] args]
        (p/resolved (mapv (fn [{:keys [id opts]}]
                            (assoc (worker-block-handler/get-block-and-children db (if (string? id) (uuid id) id) opts)
                                   :id id))
                          requests)))
      :thread-api/export-get-blocks-data
      (let [[ids opts cc] args] (p/resolved (worker-export/get-blocks-export-data db ids opts cc)))
      :thread-api/export-blocks-as-format
      (let [[ids fmt opts cc] args] (p/resolved (worker-export/export-blocks-as-format db ids fmt opts cc)))
      (p/resolved nil))))

(defn- apply-ops-in-process! [_db ops opts]
  (outliner-op/apply-ops! (conn/get-db test-helper/test-db false) ops (dissoc opts :editor/edit-block-fn)))

(def ^:private paste-blocks* editor/paste-blocks)

(defn- <round-trip
  "Copies the page's root blocks, pastes them after the target page's block.
  Returns the clipboard payload and the mismatches."
  [{:keys [page target-page]}]
  (let [db (conn/get-db test-helper/test-db)
        src-page (find-page db (:block/title page))
        roots (vec (child-blocks src-page))
        target (first (child-blocks (find-page db target-page)))
        clipboard (atom nil)]
    (p/with-redefs [state/<invoke-db-worker worker-stub
                    db-transact/apply-outliner-ops apply-ops-in-process!
                    export-html/export-blocks-as-html (fn [& _] (p/resolved ""))
                    ;; the browser clipboard: keep what the app writes, the custom type included
                    util/write-clipboard (fn [data & _] (reset! clipboard (js->clj data :keywordize-keys true)))
                    util/stop (constantly nil)
                    paste-handler/get-copied-blocks (fn [] (p/resolved (some-> (:blocks @clipboard) common-util/safe-read-map-string)))
                    ;; the editor gives paste its target: the block being edited
                    editor/paste-blocks (fn [blocks opts]
                                          (paste-blocks* blocks (assoc opts :target-block target :sibling? true :ops-only? true)))]
      (p/let [_ (editor/copy-selection-blocks true :selected-ids (mapv :block/uuid roots))
              payload @clipboard
              _ (#'paste-handler/paste-copied-blocks-or-text nil (:text payload) nil (:html payload))]
        (let [db' (conn/get-db test-helper/test-db)
              pasted (rest (child-blocks (find-page db' target-page)))]
          {:payload payload
           :pasted-count (count (tree-views pasted 0))
           :mismatches (cond
                         (empty? roots) [{:mismatch :oracle/no-source-blocks}]
                         (not (:blocks payload)) [{:mismatch :copy/no-blocks-on-clipboard}]
                         (some (set (map :block/uuid roots)) (map :block/uuid pasted)) [{:mismatch :paste/kept-source-uuid}]
                         :else (compare-views (tree-views (map #(d/entity db' (:db/id %)) roots) 0)
                                              (tree-views pasted 0)))})))))

(defn- example-size [{:keys [page]}] (count (pr-str (:blocks page))))

(deftest copy-then-paste-fuzz
  (async done
         (state/set-current-repo! test-helper/test-db)
         (let [started (js/Date.now)]
           (-> (p/loop [ps pages found {} pasted-total 0]
                 (if-let [page (first ps)]
                   (p/let [{:keys [payload mismatches pasted-count]}
                           (-> (<round-trip page)
                               (p/catch (fn [e] {:mismatches [{:mismatch :crash/round-trip
                                                               :message (str e " " (pr-str (ex-data e)))}]})))
                           found' (reduce (fn [m {:keys [mismatch] :as x}]
                                            (let [k [(:kind page) mismatch]
                                                  ex (assoc x :page page :payload payload)
                                                  prev (get-in m [k :example])]
                                              (-> m
                                                  (update-in [k :count] (fnil inc 0))
                                                  (assoc-in [k :example]
                                                            (if (and prev (<= (example-size prev) (example-size ex))) prev ex)))))
                                          found mismatches)]
                     (p/recur (rest ps) found' (+ pasted-total (or pasted-count 0))))
                   (do
                     (println "FUZZ pages" (count pages) "pasted blocks" pasted-total "seeds per kind" fuzz-n "from" fuzz-seed
                              "mismatches" (count found) "ms" (- (js/Date.now) started))
                     (doseq [[[kind mismatch] {n :count example :example}] (sort-by (comp str key) found)]
                       (println "\nKIND" (name kind) "MISMATCH" mismatch "count" n)
                       (println "  input:  " (pr-str (select-keys (:page example) [:page :blocks])))
                       (when (:message example) (println "  message:" (:message example)))
                       (when-let [s (:blocks (:payload example))]
                         (println "  clipboard:" (subs s 0 (min 2500 (count s)))))
                       (when (:src example) (println "  before: " (pr-str (:src example))))
                       (when (:dst example) (println "  after:  " (pr-str (:dst example)))))
                     (is (empty? found) "copy then paste gives back the same blocks"))))
               (p/catch (fn [e] (is false (str "fuzz crashed: " e " " (pr-str (ex-data e))))))
               (p/finally done)))))
