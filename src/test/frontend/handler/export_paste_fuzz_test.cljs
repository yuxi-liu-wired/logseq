(ns frontend.handler.export-paste-fuzz-test
  "Metamorphic test of the app's own text loop: blocks exported as Markdown
  text by the app, pasted back by the app. The pasted blocks must be the
  blocks that were exported: tree shape, title, heading, status, priority,
  display type, collapsed, tags, properties.
  Pages are small and each fuzzes one input kind (a property type, status,
  code block, block ref, titles full of Markdown specials, ...), so a mismatch
  belongs to one kind and its example is small; the kind :mixed combines all.
  Mismatches are grouped by page kind and mismatch, each with its smallest
  example. Size: FUZZ_N seeds per kind (default 10), FUZZ_SEED first seed (default 1)."
  (:require [cljs.test :refer [async deftest is use-fixtures]]
            [clojure.string :as string]
            [datascript.core :as d]
            [datascript.impl.entity :as de]
            [frontend.db.async :as db-async]
            [frontend.db.conn :as conn]
            [frontend.handler.db-based.editor :as db-editor-handler]
            [frontend.handler.editor :as editor-handler]
            [frontend.handler.export.text :as export-text]
            [frontend.handler.paste :as paste-handler]
            [frontend.state :as state]
            [frontend.test.helper :as test-helper]
            [frontend.worker.export :as worker-export]
            [logseq.db.frontend.content :as db-content]
            [logseq.db.frontend.property :as db-property]
            [logseq.db.sqlite.build :as sqlite-build]
            [promesa.core :as p]))

(defn- env-int [k default]
  (or (some-> (aget js/process.env k) js/parseInt) default))

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

;; ---------------------------------------------------------------------------
;; inputs

(def ^:private ref-pages ["Alpha" "Beta Gamma" "日本語" "ns/child"])
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
   "\n  - " "\n\n" "&" "<" ">"])

(def ^:private chat-titles
  "Inputs tried in chat, kept verbatim."
  ["^^[omega]^^" "^^ω^^" "(=^･ω･^=)" "ฅ^•ﻌ•^ฅ" "OwO"])

(defn- word-title [r] (pick r words))

(def ^:private fuzz-all? (= "1" (some-> js/process .-env .-FUZZ_ALL)))

(defn- special-title [r]
  (cond-> (string/join (repeatedly (inc (next! r 5))
                                   #(if (chance r 50)
                                      (let [s (pick r specials)]
                                        ;; a multi-line piece (a fence, a table) is on its own lines, as typed
                                        (if (and (string/includes? s "\n") (not (string/blank? s))) (str "\n" s "\n") s))
                                      (str (pick r words) " "))))
    ;; an embed macro in text is asked upstream (logseq/logseq#13755); FUZZ_ALL=1 to include
    (not fuzz-all?) (string/replace "{{embed [[Alpha]]}}" "")))

(def ^:private choice-uuids
  {:p-choice [#uuid "00000000-0000-4000-8000-0000000000c1" #uuid "00000000-0000-4000-8000-0000000000c2"]
   :p-choices [#uuid "00000000-0000-4000-8000-0000000000c3" #uuid "00000000-0000-4000-8000-0000000000c4"]})

(def ^:private property-types
  {:p-text :default :p-number :number :p-check :checkbox :p-url :url :p-date :date :p-node :node
   :p-choice :default :p-choices :default})

(defn- gen-value [r k]
  (case k
    :p-choice [:block/uuid (pick r (:p-choice choice-uuids))]
    :p-choices (set (map (fn [u] [:block/uuid u]) (some-of r (:p-choices choice-uuids))))
    ;; one line, from one-line pieces: a `key:: value` line holds no line break (a format
    ;; limit, issues/ROUNDTRIP.md); a multi-line piece glued onto one line (```clojure (x 1)
    ;; ```) is markup mldoc reads differently from what was meant
    :p-text (if (chance r 50)
              (word-title r)
              ;; a text value is not blank (the app trims a value typed as spaces)
              (let [v (string/join (repeatedly (inc (next! r 5))
                                               #(if (chance r 50)
                                                  (pick r (remove (fn [s] (string/includes? s "\n")) specials))
                                                  (str (pick r words) " "))))]
                (if (string/blank? v) (word-title r) v)))
    :p-number (pick r [0 1 -2 3.5 1000000 0.1])
    :p-check (chance r 50)
    :p-url (pick r ["https://logseq.com" "http://example.com/a?b=c" "https://x.y/a(b)c"])
    :p-date [:build/page {:build/journal (pick r [20240101 20251231])}]
    :p-node (set (map (fn [t] [:build/page {:block/title t}]) (some-of r ref-pages)))))

(def ^:private kinds
  (cond->> (into [:chat :title :heading :status :priority :tag :code :quote :math :block-ref :embed :collapsed :nesting]
                 (concat (keys property-types) [:mixed]))
    ;; embed macros in text: asked upstream (logseq/logseq#13755); FUZZ_ALL=1 to include
    (not fuzz-all?) (remove #{:embed})
    true vec))

(defn- block-for-kind
  "A block that exercises kind; seen holds the uuids of earlier blocks of the page."
  [r kind seen depth]
  (let [uuid (random-uuid)
        mixed? (= kind :mixed)
        on? (fn [k pct] (or (= kind k) (and mixed? (chance r pct))))
        code? (on? :code 8) math? (and (not code?) (on? :math 5)) quote? (and (not code?) (not math?) (on? :quote 5))
        ref-token (fn [] (when (seq @seen)
                           (cond
                             (on? :block-ref 20) (str "[[" (pick r @seen) "]]")
                             (and fuzz-all? (on? :embed 10)) (str "{{embed [[" (pick r @seen) "]]}}"))))
        title (cond
                code? (pick r ["(println \"hi\")\n(+ 1 2)" "SELECT 1;" "  indented\n\ttab" "- not a block\n# not a heading"])
                math? (pick r ["E = mc^2" "\\frac{a}{b}"])
                (= kind :chat) (pick r chat-titles)
                :else (str (if (on? :title 40) (special-title r) (word-title r))
                           (some->> (ref-token) (str " "))))
        _ (swap! seen conj uuid)
        props (into {} (for [k (keys property-types) :when (on? k 15)] [k (gen-value r k)]))
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
               code? (conj :logseq.class/Code-block)
               quote? (conj :logseq.class/Quote-block)
               math? (conj :logseq.class/Math-block))
        nest? (and (< depth 3) (or (and (= kind :nesting) (chance r 60)) (and mixed? (chance r 30))))
        ;; an ordinary block is stored as saving its typed text stores it (title,
        ;; heading); with refs the typed title is kept, and the build links them
        saved (when-not (or code? math? quote?)
                (try (db-editor-handler/wrap-parse-block {:block/title title})
                     (catch :default _ nil)))
        title (if (and saved (empty? (:block/refs saved))) (:block/title saved) title)
        built-ins (cond-> built-ins
                    (and saved (:logseq.property/heading saved) (not (:logseq.property/heading built-ins)))
                    (assoc :logseq.property/heading (:logseq.property/heading saved)))]
    (cond-> {:block/title title
             :block/uuid uuid
             :build/keep-uuid? true}
      (or (seq props) (seq built-ins)) (assoc :build/properties (merge props built-ins))
      (seq tags) (assoc :build/tags tags)
      (on? :collapsed 10) (assoc :block/collapsed? true)
      nest? (assoc :build/children (vec (repeatedly (inc (next! r 3)) #(block-for-kind r kind seen (inc depth))))))))

(defn- gen-page [kind seed]
  (let [r (rng (+ seed (* 7919 (.indexOf kinds kind))))
        seen (atom [])]
    {:kind kind
     :page {:block/title (str "fuzz " (name kind) " " seed)}
     :blocks (vec (repeatedly (inc (next! r 3)) #(block-for-kind r kind seen 0)))}))

(defn- pre-order [blocks depth]
  (mapcat (fn [b] (cons [b depth] (pre-order (:build/children b) (inc depth)))) blocks))

;; ---------------------------------------------------------------------------
;; readable forms and the oracle

(defn- readable-value [v]
  (cond
    (set? v) (set (map readable-value v))
    (:block/journal-day v) (:block/journal-day v)
    (:db/id v) (or (some-> v :block/title str) (str (db-property/property-value-content v)))
    :else v))

(defn- ident-name [v]
  (cond (keyword? v) (name v) (map? v) (some-> (:db/ident v) name) :else v))

(defn- source-view [db b depth]
  (let [e (d/entity db [:block/uuid (:block/uuid b)])]
    {:depth depth
     :title (db-content/recur-replace-uuid-in-block-title e)
     :heading (:logseq.property/heading e)
     :status (ident-name (:logseq.property/status e))
     :priority (ident-name (:logseq.property/priority e))
     :display-type (:logseq.property.node/display-type e)
     ;; collapsed means something only for a block with children
     :collapsed? (boolean (and (:block/collapsed? e) (seq (:build/children b))))
     ;; the pipeline adds the display-type tags (Code, Math, Quote) from the display type
     :tags (set (map :block/title (remove #(contains? #{:logseq.class/Code-block :logseq.class/Math-block
                                                       :logseq.class/Quote-block} (:db/ident %))
                                          (:block/tags e))))
     :props (into {} (for [[k v] (db-property/properties e)
                           :when (= "user.property" (namespace k))]
                       [(name k) (readable-value v)]))}))

(defn- pasted-view
  "The pasted block as it would be stored: the block plus the property
  assignments paste hands to the insert ({property-ident value}, values as
  entity ids)."
  [db b assignments has-children?]
  (let [uuid->title (into {} (keep (fn [x] (when (map? x) [(str (:block/uuid x)) (:block/title x)]))) (:block/refs b))
        title (string/replace (str (:block/title b)) db-content/id-ref-pattern
                              (fn [[m id]] (if-let [t (uuid->title id)] (str "[[" t "]]") m)))
        b (merge b assignments)
        resolve (fn [v] (cond
                          (integer? v) (or (d/entity db v) v)
                          (sequential? v) (set (map #(if (integer? %) (or (d/entity db %) %) %) v))
                          :else v))]
    {:depth (dec (or (:block/level b) 1))
     :title title
     :heading (:logseq.property/heading b)
     :status (ident-name (resolve (:logseq.property/status b)))
     :priority (ident-name (resolve (:logseq.property/priority b)))
     :display-type (:logseq.property.node/display-type b)
     :collapsed? (boolean (and (:block/collapsed? b) has-children?))
     :tags (set (keep #(cond (map? %) (:block/title %) (de/entity? %) (:block/title %))
                      (let [t (resolve (:block/tags b))] (if (set? t) t (when t [t])))))
     ;; only ref-valued properties are handed over as entity ids
     :props (into {} (for [[k v] b :when (= "user.property" (namespace k))]
                       [(name k) (readable-value (if (contains? #{"p-date" "p-node" "p-choice" "p-choices"} (name k))
                                                   (resolve v)
                                                   v))]))
     :other-keys (set (remove #{:block/title :block/uuid :block/level :block/parent :block/order :block/page
                                :block/refs :block/format}
                              (keys b)))}))

(defn- org-quote? [s] (boolean (re-find #"(?i)^#\+BEGIN_QUOTE" (string/trim (str s)))))

(def accepted
  "Named rules for differences recorded in issues/ROUNDTRIP.md as not bugs (same
  display) or asked upstream; each hit is counted and printed."
  (atom {}))

(def ^:private title-rules
  [[:whitespace "outer and line-end whitespace (Markdown hard breaks)"
    #(-> % (string/replace #"(?m)[ \t]+$" "") string/trim)]
   [:inline-math "\\(x\\) written as $x$, spaces around inline math"
    #(-> % (string/replace #"\\[()]" "$$") (string/replace #"\s*(\$+)\s*" "$1"))]
   [:highlight "==m== written as ^^m^^" #(string/replace % "==" "^^")]
   [:table-separator "a table separator row with no header row (mldoc: empty table)"
    #(string/replace % #"(?m)^\|[-|: ]+\|\n?" "")]
   [:table-cells "table cells written with a space around their text (same table)"
    (fn [s]
      (->> (string/split s "\n")
           (map (fn [line]
                  (if (string/starts-with? line "|")
                    (string/join "|" (map string/trim (string/split line "|" -1)))
                    line)))
           (string/join "\n")))]
   [:org-tag "#+X in text is a tag, as saving makes it" #(string/replace % #"#\[\[(\+[A-Za-z_]+)\]\]" "#$1")]
   [:tag-brackets "#tag written as #[[tag]] (same tag)" #(string/replace % #"#\[\[([^\]\s]+)\]\]" "#$1")]])

(defn- same-title?
  "Whether two titles differ only by accepted rules; counts the rules used."
  [a b]
  (or (= a b)
      ;; twice over, so a rule's leftovers meet the earlier rules
      (loop [[[rule _ f] & more] (concat title-rules title-rules) a a b b used []]
        (cond
          (= a b) (do (doseq [r used] (swap! accepted update r (fnil inc 0))) true)
          (nil? rule) false
          :else (let [a' (f a) b' (f b)]
                  (recur more a' b' (cond-> used (or (not= a a') (not= b b')) (conj rule))))))))

(defn- embed-macro? [s] (string/includes? (str s) "{{embed [["))

(defn- mismatch-kinds [src dst]
  (let [;; an Org quote written in a block shows as a quote; it may come back as a quote block
        [src dst] (if (and (org-quote? (:title src)) (= :quote (:display-type dst)))
                    [(assoc src :title (:title dst) :display-type :quote) dst]
                    [src dst])
        plain? (and (empty? (:props src)) (empty? (:tags src)) (nil? (:heading src)) (nil? (:status src))
                    (nil? (:priority src)) (nil? (:display-type src)))
        ;; an embed macro in text: asked upstream, https://github.com/logseq/logseq/issues/13755
        same? (or (same-title? (str (:title src)) (str (:title dst)))
                  (and (embed-macro? (:title src))
                       (do (swap! accepted update :embed-macro-13755 (fnil inc 0)) true)))]
    (cond-> []
      (not= (:depth src) (:depth dst)) (conj :structure/depth)
      (and (not same?) plain?) (conj :title/plain-block)
      (and (not same?) (not plain?)) (conj :title/block-with-metadata)
      (not= (:heading src) (:heading dst)) (conj :heading)
      (not= (:status src) (:status dst)) (conj :status)
      (not= (:priority src) (:priority dst)) (conj :priority)
      (not= (:display-type src) (:display-type dst))
      (conj (keyword "display-type" (str (or (some-> (:display-type src) name) "none") "->"
                                         (or (some-> (:display-type dst) name) "none"))))
      (not= (:collapsed? src) (:collapsed? dst)) (conj :collapsed)
      (not= (:tags src) (:tags dst)) (conj :tags)
      ;; values compared as trimmed text: a number may come back as a number for
      ;; its text, and outer spaces of a text value are trimmed
      true (into (for [[k v] (:props src)
                       :let [norm (fn [x] (if (set? x) (set (map #(string/trim (str %)) x)) (string/trim (str x))))
                             dv (get (:props dst) k)]
                       :when (and (not= (norm v) (norm dv))
                                  (not (and (string? v) (string? dv) (same-title? v dv))))]
                   (keyword "property" k)))
      true (into (for [k (keys (:props dst)) :when (not (contains? (:props src) k))]
                   :property/extra)))))

;; ---------------------------------------------------------------------------

(def ^:private pages
  (vec (for [kind kinds seed (range fuzz-seed (+ fuzz-seed fuzz-n))] (gen-page kind seed))))

(use-fixtures :once
  {:before (fn []
             (test-helper/start-test-db!)
             (let [{:keys [init-tx block-props-tx]}
                   (sqlite-build/build-blocks-tx
                    {:pages-and-blocks (-> (mapv (fn [t] {:page {:block/title t}}) ref-pages)
                                           (into (map (fn [day] {:page {:build/journal day}}) [20240101 20251231]))
                                           (into (map #(select-keys % [:page :blocks]) pages)))
                     :classes (into {} (map (fn [c] [c {}]) classes))
                     :properties
                     (into {} (for [[k t] property-types]
                                [k (cond-> {:logseq.property/type t}
                                     (#{:p-node :p-choices} k) (assoc :db/cardinality :db.cardinality/many)
                                     (= k :p-choice) (assoc :build/closed-values
                                                            (mapv (fn [v u] {:value v :uuid u}) ["joy" "sad"] (:p-choice choice-uuids)))
                                     (= k :p-choices) (assoc :build/closed-values
                                                             (mapv (fn [v u] {:value v :uuid u}) ["red" "blue"] (:p-choices choice-uuids))))]))})]
               (d/transact! (conn/get-db test-helper/test-db false)
                            (concat (map #(select-keys % [:block/uuid]) init-tx) init-tx block-props-tx))))
   :after test-helper/destroy-test-db!})

(defn- worker-stub [api repo & args]
  (let [db (conn/get-db repo)]
    (case api
      :thread-api/export-get-blocks-data
      (let [[ids opts cc] args] (p/resolved (worker-export/get-blocks-export-data db ids opts cc)))
      :thread-api/export-blocks-as-format
      (let [[ids fmt opts cc] args] (p/resolved (worker-export/export-blocks-as-format db ids fmt opts cc)))
      :thread-api/pull
      (let [[selector [a v :as lookup]] args]
        (p/resolved (try (if (= :block/name a)
                           (some->> (d/q '[:find ?e . :in $ ?n :where [?e :block/name ?n] [(missing? $ ?e :block/parent)]] db v)
                                    (d/pull db selector))
                           (d/pull db selector lookup))
                         (catch :default _ nil))))
      :thread-api/q
      (let [[[query & inputs]] args] (p/resolved (apply d/q query db inputs)))
      (p/resolved nil))))

(defn- round-trip [page]
  (let [pasted (atom nil)
        db (conn/get-db test-helper/test-db)
        roots (mapv :block/uuid (:blocks page))]
    (p/with-redefs [state/<invoke-db-worker worker-stub
                    state/get-edit-block (constantly {:db/id 10})
                    db-async/<get-block-page-info (fn [& _] (p/resolved {:db/id 1 :block/name "paste target"}))
                    editor-handler/paste-blocks (fn [blocks opts] (reset! pasted [blocks opts]) (p/resolved nil))]
      (p/let [text (export-text/export-blocks-as-markdown test-helper/test-db roots {:remove-options #{}})
              _ (#'paste-handler/paste-text-parseable :markdown text)]
        (let [src (map (fn [[b depth]] (source-view db b depth)) (pre-order (:blocks page) 0))
              [blocks {:keys [property-assignments]}] @pasted
              by-block (reduce (fn [m [u k v]] (assoc-in m [u k] v)) {} property-assignments)
              levels (mapv #(or (:block/level %) 1) blocks)
              dst (map-indexed (fn [i b]
                                 (pasted-view db b (get by-block (:block/uuid b))
                                              (> (get levels (inc i) 0) (get levels i))))
                               blocks)]
          {:text text
           :mismatches (if (not= (count src) (count dst))
                         [{:mismatch :structure/block-count
                           :src (mapv (juxt :depth :title :status :props) src)
                           :dst (mapv (juxt :depth :title :status :props) dst)}]
                         (for [[s t] (map vector src dst)
                               m (mismatch-kinds s t)]
                           {:mismatch m :src s :dst t}))})))))

(deftest export-then-paste-fuzz
  (async done
         (state/set-current-repo! test-helper/test-db)
         (-> (p/loop [ps pages found {}]
               (if-let [page (first ps)]
                 (p/let [{:keys [text mismatches]} (round-trip page)
                         found' (reduce (fn [m {:keys [mismatch] :as x}]
                                          (let [k [(:kind page) mismatch]
                                                ex (assoc x :text text :page (get-in page [:page :block/title]))
                                                prev (get-in m [k :example])]
                                            (-> m
                                                (update-in [k :count] (fnil inc 0))
                                                ;; every distinct before -> after pair, so one
                                                ;; small example cannot hide other causes in a kind
                                                (update-in [k :pairs] (fnil conj #{}) [(:src x) (:dst x) text])
                                                (assoc-in [k :example]
                                                          (if (and prev (<= (count (:text prev)) (count text))) prev ex)))))
                                        found mismatches)]
                   (p/recur (rest ps) found'))
                 (do
                   (println "FUZZ kinds" (count kinds) "seeds per kind" fuzz-n "from" fuzz-seed "pages" (count pages)
                            "mismatches" (count found))
                   (println "ACCEPTED (rules for recorded not-bugs and asked cases):" (pr-str (into (sorted-map) @accepted)))
                   (doseq [[[kind mismatch] {:keys [count example pairs]}] (sort-by (comp str key) found)]
                     (println "\nKIND" (name kind) "MISMATCH" mismatch "count" count "page" (:page example))
                     (println "  exported:" (pr-str (:text example)))
                     (println "  before:  " (pr-str (:src example)))
                     (println "  after:   " (pr-str (:dst example)))
                     (doseq [[src dst text] (sort-by (comp clojure.core/count pr-str (partial take 2)) pairs)]
                       (println "  PAIR" (pr-str (if (map? src) (:title src) src)) "->" (pr-str (if (map? dst) (:title dst) dst))
                                "via" (pr-str text))))
                   (is (empty? found) "export then paste gives back the same blocks"))))
             (p/catch (fn [e] (is false (str "fuzz crashed: " e " " (pr-str (ex-data e))))))
             (p/finally done))))
