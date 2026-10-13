(ns logseq.db.sqlite.export-import-fuzz-test
  "Metamorphic test of the DB graph's EDN backup loop: a graph exported by
  `sqlite-export/build-export` and imported by `sqlite-export/build-import`
  into a fresh graph must give back the same pages and blocks: tree shape,
  title (refs shown as titles), heading, status, priority, display type,
  embed (:block/link), collapsed (blocks with children), tags, user
  properties, and for pages also tags, properties and aliases.
  One source graph per input kind holds one small page per seed, so a mismatch
  belongs to one kind and its example is small; the kind :mixed combines all.
  Each graph goes through every export type: :graph and :graph-human (the
  whole graph), :page (each page), :selected-nodes (each page's root blocks
  with their children, the app's \"export blocks\") and :block (each block
  alone, imported into an empty block). Mismatches are grouped by export type,
  page kind and mismatch, each with its smallest example.
  Env: FUZZ_N seeds per kind (default 10), FUZZ_SEED first seed (default 1),
  FUZZ_KINDS and FUZZ_TYPES comma-separated subsets (default all)."
  (:require [cljs.test :refer [deftest is]]
            [clojure.string :as string]
            [datascript.core :as d]
            [datascript.impl.entity :as de]
            [logseq.db.frontend.content :as db-content]
            [logseq.db.frontend.property :as db-property]
            [logseq.db.frontend.validate :as db-validate]
            [logseq.db.sqlite.export :as sqlite-export]
            [logseq.db.test.helper :as db-test]))

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
;; inputs

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
         :collapsed :nesting :pvalue-block :page-props :journal :alias]
        (concat (keys property-types) [:mixed])))

(def ^:private export-types [:graph :graph-human :page :selected-nodes :block])

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

(defn- gen-page
  "A page of the given kind: :page and :blocks as in sqlite.build, :extra-pages
  for pages it needs (aliases)."
  [kind seed]
  (let [r (rng (+ seed (* 7919 (.indexOf kinds kind))))
        seen (atom [])
        mixed? (= kind :mixed)
        on? (fn [k pct] (or (= kind k) (and mixed? (chance r pct))))
        page-props (when (on? :page-props 20)
                     (into {} (for [k (some-of r (keys property-types))] [k (gen-value r k)])))
        page-tags (when (on? :page-props 20) (some-of r classes))
        alias-uuid (when (on? :alias 15) (rng-uuid r))
        page (if (= kind :journal)
               {:build/journal (+ 20230101 (mod seed 28))}
               (cond-> {:block/title (str "fuzz " (name kind) " " seed)}
                 (seq page-props) (assoc :build/properties page-props)
                 (seq page-tags) (assoc :build/tags page-tags)
                 alias-uuid (assoc :block/alias #{[:block/uuid alias-uuid]})))]
    {:kind kind
     :page page
     :extra-pages (when alias-uuid
                    [{:page {:block/title (str "alias " (name kind) " " seed) :block/uuid alias-uuid :build/keep-uuid? true}}])
     :blocks (vec (repeatedly (inc (next! r 3)) #(block-for-kind r kind seen 0)))}))

(defn- source-options [pages]
  {:pages-and-blocks (-> (mapv (fn [t] {:page {:block/title t}}) ref-pages)
                         (into (map (fn [day] {:page {:build/journal day}}) journal-days))
                         (into (mapcat :extra-pages pages))
                         (into (map #(select-keys % [:page :blocks]) pages)))
   :classes (into {} (map (fn [c] [c {}]) classes))
   :properties
   (into {} (for [[k t] property-types]
              [k (cond-> {:logseq.property/type t}
                   (many-properties k) (assoc :db/cardinality :db.cardinality/many)
                   (= k :p-choice) (assoc :build/closed-values
                                          (mapv (fn [v u] {:value v :uuid u}) ["joy" "sad"] (:p-choice choice-uuids)))
                   (= k :p-choices) (assoc :build/closed-values
                                           (mapv (fn [v u] {:value v :uuid u}) ["red" "blue"] (:p-choices choice-uuids))))]))})

;; ---------------------------------------------------------------------------
;; readable forms of a graph, the same on both sides

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

(defn- tree-views [parent depth]
  (mapcat (fn [b] (cons (block-view b depth) (tree-views b (inc depth)))) (child-blocks parent)))

(defn- page-view [page]
  {:depth -1
   :title (:block/title page)
   :tags (set (map :block/title (:block/tags page)))
   :alias (set (map :block/title (:block/alias page)))
   :props (user-props page)})

(defn- find-page [db {:keys [block/title build/journal]}]
  (some->> (if journal
             (d/q '[:find ?p . :in $ ?day :where [?p :block/journal-day ?day]] db journal)
             (d/q '[:find ?p . :in $ ?t :where [?p :block/title ?t] [?p :block/name]] db title))
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
      (not= (:alias src) (:alias dst)) (conj :page/alias)
      true (into (for [[k v] (:props src) :when (not= v (get (:props dst) k))]
                   (keyword "property" k)))
      true (into (for [k (keys (:props dst)) :when (not (contains? (:props src) k))]
                   :property/extra)))))

(defn- compare-views [src dst]
  (if (not= (count src) (count dst))
    [{:mismatch :structure/block-count :src (vec src) :dst (vec dst)}]
    (for [[s t] (map vector src dst)
          m (mismatch-kinds s t)]
      {:mismatch m :src s :dst t})))

;; ---------------------------------------------------------------------------
;; the loop

(defn- import-into-fresh-graph
  "Imports export-map into a fresh graph; returns the conn or a failure map."
  [export-map & {:keys [current-block-title]}]
  (let [conn (if current-block-title
               (db-test/create-conn-with-blocks {:pages-and-blocks [{:page {:block/title "import target"}
                                                                     :blocks [{:block/title current-block-title}]}]})
               (db-test/create-conn))
        current-block (when current-block-title (db-test/find-block-by-content @conn current-block-title))
        result (try (sqlite-export/build-import export-map @conn (cond-> {} current-block (assoc :current-block current-block)))
                    (catch :default e {:crash (str "build-import: " (ex-message e))}))]
    (cond
      (:crash result) {:failure :crash/import :message (:crash result)}
      (:error result) {:failure :import/error :message (:error result)}
      :else
      (if-let [err (try (d/transact! conn (concat (:init-tx result) (:block-props-tx result) (:misc-tx result))) nil
                        (catch :default e (str "transact: " (ex-message e))))]
        {:failure :crash/import :message err}
        (let [errors (:errors (db-validate/validate-local-db! @conn))]
          (if (seq errors)
            {:failure :import/invalid-graph :message (pr-str (take 2 errors)) :conn conn}
            {:conn conn :current-block current-block}))))))

(defn- export-safe [db opts]
  (try {:export (sqlite-export/build-export db opts)}
       (catch :default e {:failure :crash/export :message (str "build-export: " (ex-message e))})))

(defn- failure-result [{:keys [failure message]}]
  [{:mismatch failure :message message}])

(defn- page-results
  "Compares each source page with the same page in the imported graph."
  [src-db dst-db pages]
  (for [page pages]
    (let [src-page (find-page src-db (:page page))
          dst-page (find-page dst-db (:page page))]
      {:page page
       :mismatches (if-not dst-page
                     [{:mismatch :page/missing}]
                     (compare-views (cons (page-view src-page) (tree-views src-page 0))
                                    (cons (page-view dst-page) (tree-views dst-page 0))))})))

(defn- graph-results [export-type src-db pages]
  (let [{:keys [export failure] :as ex} (export-safe src-db (cond-> {:export-type export-type}
                                                              (= export-type :graph-human) (assoc :graph-options {})))]
    (if failure
      (map (fn [page] {:page page :mismatches (failure-result ex)}) pages)
      (let [{:keys [conn failure] :as im} (import-into-fresh-graph export)
            results (if conn (page-results src-db @conn pages) (map (fn [page] {:page page :mismatches []}) pages))]
        (cond->> results
          failure (map (fn [r] (update r :mismatches #(into (vec (failure-result im)) %)))))))))

(defn- page-export-result [src-db page]
  (let [src-page (find-page src-db (:page page))
        {:keys [export failure] :as ex} (export-safe src-db {:export-type :page :page-id (:db/id src-page)})]
    (if failure
      {:page page :mismatches (failure-result ex)}
      (let [{:keys [conn failure] :as im} (import-into-fresh-graph export)]
        {:page page
         :export export
         :mismatches (concat (when failure (failure-result im))
                             (when conn (:mismatches (first (page-results src-db @conn [page])))))}))))

(defn- selected-nodes-result [src-db page]
  (let [src-page (find-page src-db (:page page))
        roots (child-blocks src-page)
        {:keys [export failure] :as ex} (export-safe src-db {:export-type :selected-nodes :node-ids (mapv :db/id roots)})]
    (if failure
      {:page page :mismatches (failure-result ex)}
      (let [{:keys [conn failure] :as im} (import-into-fresh-graph export)
            dst-page (when conn (find-page @conn (:page page)))]
        {:page page
         :export export
         :mismatches (concat (when failure (failure-result im))
                             (when conn
                               (if-not dst-page
                                 [{:mismatch :page/missing}]
                                 (compare-views (tree-views src-page 0) (tree-views dst-page 0)))))}))))

(defn- block-result
  "Each block alone (the :block export has no children), imported into an empty block."
  [src-db page]
  (let [src-page (find-page src-db (:page page))]
    (for [[src-block depth] (mapcat (fn walk [[b d]] (cons [b d] (mapcat #(walk [% (inc d)]) (child-blocks b))))
                                    (map #(vector % 0) (child-blocks src-page)))]
      (let [{:keys [export failure] :as ex} (export-safe src-db {:export-type :block :block-id (:db/id src-block)})]
        (if failure
          {:page page :mismatches (failure-result ex)}
          (let [{:keys [conn failure] :as im} (import-into-fresh-graph export :current-block-title "import target block")
                dst-block (when conn (d/entity @conn (:db/id (:current-block im))))]
            {:page page
             :export export
             :mismatches (concat (when failure (failure-result im))
                                 (when dst-block
                                   (compare-views [(assoc (block-view src-block depth) :depth 0 :children? false)]
                                                  [(block-view dst-block 0)])))}))))))

(defn- kind-results [export-types kind]
  (let [pages (vec (for [seed (range fuzz-seed (+ fuzz-seed fuzz-n))] (gen-page kind seed)))
        conn (try (db-test/create-conn-with-blocks (source-options pages))
                  (catch :default e e))]
    (if (instance? js/Error conn)
      [{:export-type :source :page (first pages)
        :mismatches [{:mismatch :crash/build-source :message (ex-message conn)}]}]
      (let [src-db @conn]
        (for [export-type export-types
              result (case export-type
                       (:graph :graph-human) (graph-results export-type src-db pages)
                       :page (map #(page-export-result src-db %) pages)
                       :selected-nodes (map #(selected-nodes-result src-db %) pages)
                       :block (mapcat #(block-result src-db %) pages))]
          (assoc result :export-type export-type))))))

(defn- example-size [{:keys [page]}] (count (pr-str (select-keys page [:page :blocks]))))

(deftest export-then-import-fuzz
  (let [kinds' (env-keywords "FUZZ_KINDS" kinds)
        types' (env-keywords "FUZZ_TYPES" export-types)
        started (js/Date.now)
        found (reduce
               (fn [found kind]
                 (reduce (fn [m {:keys [mismatches export-type] :as result}]
                           (reduce (fn [m {:keys [mismatch] :as x}]
                                     (let [k [export-type kind mismatch]
                                           ex (merge (select-keys result [:page :export]) x)
                                           prev (get-in m [k :example])]
                                       (-> m
                                           (update-in [k :count] (fnil inc 0))
                                           (assoc-in [k :example]
                                                     (if (and prev (<= (example-size prev) (example-size ex))) prev ex)))))
                                   m mismatches))
                         found (kind-results types' kind)))
               {} kinds')]
    (println "FUZZ kinds" (count kinds') "export types" (pr-str types') "seeds per kind" fuzz-n "from" fuzz-seed
             "mismatches" (count found) "ms" (- (js/Date.now) started))
    (doseq [[[export-type kind mismatch] {n :count example :example}] (sort-by (comp str key) found)]
      (println "\nTYPE" (name export-type) "KIND" (name kind) "MISMATCH" mismatch "count" n)
      (println "  input:  " (pr-str (select-keys (:page example) [:page :blocks])))
      (when (:message example) (println "  message:" (:message example)))
      (when (:export example)
        (let [s (pr-str (dissoc (:export example) ::sqlite-export/export-type))]
          (println "  export: " (subs s 0 (min 1500 (count s))))))
      (when (:src example) (println "  before: " (pr-str (:src example))))
      (when (:dst example) (println "  after:  " (pr-str (:dst example)))))
    (is (empty? (sort (map str (keys found)))) "export then import gives back the same pages and blocks")))
