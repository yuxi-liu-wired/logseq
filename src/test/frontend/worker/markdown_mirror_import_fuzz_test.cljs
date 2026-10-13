(ns frontend.worker.markdown-mirror-import-fuzz-test
  "Metamorphic test of the Markdown mirror as a way to move or rebuild a graph:
  a DB graph written as Markdown files by the mirror (`markdown-mirror/<mirror-repo!`,
  the full regeneration), the files imported as a file graph into a fresh DB
  graph by the graph-parser importer (`gp-exporter/export-file-graph`, as the
  app's file graph import does) must give back the same pages and blocks: tree
  shape, title (refs shown as titles), heading, status, priority, display
  type, embed (:block/link), collapsed (blocks with children), tags, user
  properties, and for pages also tags, properties and aliases.
  One source graph per input kind holds one small page per seed, so a mismatch
  belongs to one kind and its example is small; the kind :mixed combines all.
  Mismatches are grouped by page kind and mismatch, each with its smallest
  example (the page's mirror file).
  Env: FUZZ_N seeds per kind (default 10), FUZZ_SEED first seed (default 1),
  FUZZ_KINDS comma-separated subset (default all)."
  (:require [cljs.test :refer [async deftest is]]
            [clojure.set :as set]
            [clojure.string :as string]
            [datascript.core :as d]
            [datascript.impl.entity :as de]
            [frontend.worker.markdown-mirror :as markdown-mirror]
            [logseq.db.frontend.content :as db-content]
            [logseq.db.frontend.property :as db-property]
            [logseq.db.test.helper :as db-test]
            [logseq.graph-parser.exporter :as gp-exporter]
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
   "\n  - " "\n\n" "&" "<" ">"])

(def ^:private chat-titles
  "Inputs tried in chat, kept verbatim."
  ["^^[omega]^^" "^^ω^^" "(=^･ω･^=)" "ฅ^•ﻌ•^ฅ" "OwO"])

(defn- word-title [r] (pick r words))

(defn- special-title [r]
  (let [title (string/join (repeatedly (inc (next! r 5)) #(if (chance r 50) (pick r specials) (str (pick r words) " "))))]
    (if (= "1" (some-> js/process .-env .-FUZZ_ALL))
      title
      (-> title
          ;; an embed macro in text is asked upstream (logseq/logseq#13755)
          (string/replace "{{embed [[Alpha]]}}" "")
          ;; a tag glued to text (`#[[a b]]x`): the importer's tag name takes the
          ;; text along (edge, issues/ROUNDTRIP.md)
          (string/replace #"(#\[\[[^\]]+\]\])(?=\S)" "$1 ")))))

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

(def ^:private asked-upstream
  "Content the mirror's syntax does not round-trip through the Markdown importer
  by design, pending upstream answers (issues/ROUNDTRIP.md): properties of every
  type, priority, page properties, page tags and aliases are `* key::` items the
  importer does not read (logseq/db-test#1440), and so is `* collapsed:: true`;
  an embed macro in text (logseq/logseq#13755). Not generated unless FUZZ_ALL=1."
  (into #{:priority :pvalue-block :page-props :alias :embed :collapsed} (keys property-types)))

(def ^:private fuzz-all? (= "1" (some-> js/process .-env .-FUZZ_ALL)))

(def ^:private kinds
  (cond->> (into [:chat :title :heading :status :priority :tag :code :quote :math :block-ref :embed :link
                  :collapsed :nesting :pvalue-block :page-props :journal :alias]
                 (concat (keys property-types) [:mixed]))
    (not fuzz-all?) (remove asked-upstream)
    true vec))

(defn- block-for-kind
  "A block that exercises kind; seen holds the uuids of earlier blocks of the page."
  [r kind seen depth]
  (let [uuid (rng-uuid r)
        mixed? (= kind :mixed)
        on? (fn [k pct] (and (or fuzz-all? (not (asked-upstream k)))
                             (or (= kind k) (and mixed? (chance r pct)))))
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
        on? (fn [k pct] (and (or fuzz-all? (not (asked-upstream k)))
                             (or (= kind k) (and mixed? (chance r pct)))))
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

(defn- user-props
  "User properties by property title: an import names its properties anew."
  [db e]
  (into {} (for [[k v] (db-property/properties e)
                 :when (= "user.property" (namespace k))]
             [(or (:block/title (d/entity db k)) (name k)) (readable-value v)])))

(defn- child-blocks [parent]
  (->> (:block/_parent parent)
       (remove :logseq.property/created-from-property)
       (sort-by :block/order)))

(defn- block-view [db e depth]
  {:depth depth
   ;; a block ref stays [[uuid]]: the mirror keeps block ids, and the referenced
   ;; block's own title is compared where that block is
   :title (db-content/recur-replace-uuid-in-block-title e 10 {:replace-block-refs? false})
   :heading (:logseq.property/heading e)
   :status (ident-name (:logseq.property/status e))
   :priority (ident-name (:logseq.property/priority e))
   :display-type (:logseq.property.node/display-type e)
   :link (some-> (:block/link e) db-content/recur-replace-uuid-in-block-title)
   :collapsed? (boolean (:block/collapsed? e))
   :children? (boolean (seq (child-blocks e)))
   :tags (set (map :block/title (:block/tags e)))
   :props (user-props db e)})

(defn- tree-views [db parent depth]
  (mapcat (fn [b] (cons (block-view db b depth) (tree-views db b (inc depth)))) (child-blocks parent)))

(defn- page-view [db page]
  {:depth -1
   :title (:block/title page)
   :tags (set (map :block/title (:block/tags page)))
   :alias (set (map :block/title (:block/alias page)))
   :props (user-props db page)})

(defn- find-page [db {:keys [block/title build/journal]}]
  (some->> (if journal
             (d/q '[:find ?p . :in $ ?day :where [?p :block/journal-day ?day]] db journal)
             (d/q '[:find ?p . :in $ ?n :where [?p :block/name ?n]] db (string/lower-case title)))
           (d/entity db)))

(def accepted
  "Named rules for differences recorded in issues/ROUNDTRIP.md as not bugs (same
  display) or by design; each hit is counted and printed."
  (atom {}))

(def ^:private query-macro-re #"\{\{query [^}]*\}\}")

(def ^:private title-rules
  [[:whitespace #(-> % (string/replace #"(?m)[ \t]+$" "") string/trim)]
   ;; the importer turns a {{query}} macro into a Query block (by design)
   [:query-macro #(string/replace % query-macro-re "")]])

(defn- same-title? [a b]
  (or (= a b)
      ;; twice over, so a rule's leftovers (a space where a macro was) meet the earlier rules
      (loop [[[rule f] & more] (concat title-rules title-rules) a a b b used []]
        (cond
          (= a b) (do (doseq [r used] (swap! accepted update r (fnil inc 0))) true)
          (nil? rule) false
          :else (let [a' (f a) b' (f b)]
                  (recur more a' b' (cond-> used (or (not= a a') (not= b b')) (conj rule))))))))

(defn- mismatch-kinds [src dst]
  (let [plain? (and (empty? (:props src)) (empty? (:tags src)) (nil? (:heading src)) (nil? (:status src))
                    (nil? (:priority src)) (nil? (:display-type src)))
        ;; a #name typed in the text comes back as a tag of that name, the token
        ;; removed (the importer's remove-inline-tags default; saving makes it a tag too)
        added-tags (remove #{"Query"} (set/difference (:tags dst) (:tags src)))
        tag-token (fn [t] (or (some #(when (string/includes? (str (:title src)) %) %)
                                    [(str "#[[" t "]]") (str "#" t)])
                              ;; a namespaced tag: the importer makes `#[[ns/child]]` the page `child`
                              (re-find (re-pattern (str "#\\[\\[[^\\]]*/" (string/replace t #"[.*+?^${}()|\[\]\\]" "\\\\$&") "\\]\\]"))
                                       (str (:title src)))))
        inline-tag? (and (seq added-tags)
                         (set/subset? (:tags src) (:tags dst))
                         (every? tag-token added-tags))
        _ (when inline-tag? (swap! accepted update :inline-tag (fnil inc 0)))
        src-title (if inline-tag?
                    (reduce (fn [s t] (string/replace s (tag-token t) "")) (str (:title src)) added-tags)
                    (str (:title src)))
        same? (same-title? src-title (str (:title dst)))
        ;; the importer's Query block from a {{query}} macro carries the Query tag
        query-tag? (or inline-tag?
                       (and (re-find query-macro-re (str (:title src)))
                            (= (conj (:tags src) "Query") (:tags dst))))]
    (when (and query-tag? (not inline-tag?)) (swap! accepted update :query-macro (fnil inc 0)))
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
      (not (same-title? (str (:link src)) (str (:link dst)))) (conj :link)
      (and (:children? src) (not= (:collapsed? src) (:collapsed? dst))) (conj :collapsed)
      (and (not= (:tags src) (:tags dst)) (not query-tag?)) (conj :tags)
      (not= (:alias src) (:alias dst)) (conj :page/alias)
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

(def ^:private test-repo "logseq_db_mirror-fuzz")

(defn- fake-platform
  "A node platform whose storage is the files atom."
  [files]
  {:env {:runtime :node}
   :storage {:read-text! (fn [path] (p/resolved (get @files path)))
             :write-text-atomic! (fn [path content] (swap! files assoc path content) (p/resolved nil))
             :delete-file! (fn [path] (swap! files dissoc path) (p/resolved nil))}
   :broadcast {:post-message! (fn [& _] nil)}})

(def ^:private config-edn "{:file/name-format :triple-lowbar}")

(defn- <import-mirror
  "Imports the mirror files (relative path -> content) as a file graph into a
  fresh DB graph; returns the conn and the importer's error messages."
  [mirror-files]
  (let [conn (db-test/create-conn)
        files (assoc mirror-files "logseq/config.edn" config-edn)
        notes (atom [])
        file-maps (mapv #(hash-map :path %) (sort (keys files)))
        stable-idents (some-> js/process .-env .-LOGSEQ_STABLE_IDENTS)]
    ;; import as the app does: class and property idents get their random suffix
    ;; (the test runner's stable idents make e.g. classes `Beta Gamma` and
    ;; `Beta Gamma@` collide, which the app never does)
    (js-delete (.-env js/process) "LOGSEQ_STABLE_IDENTS")
    (-> (gp-exporter/export-file-graph
         conn conn {:path "logseq/config.edn"} file-maps
         {:<read-file (fn [file] (p/resolved (get files (:path file))))
          :rpath-key :path
          :notify-user (fn [m] (when (= :error (:level m)) (swap! notes conj (str (:msg m)))))
          :user-options {:convert-all-tags? true :remove-inline-tags? true}
          :<read-and-copy-asset (fn [& _] (p/resolved nil))})
        (p/then (fn [_] {:conn conn :notes @notes}))
        (p/catch (fn [e] {:conn conn :notes (conj @notes (str "export-file-graph: " (ex-message e)))}))
        (p/finally (fn [] (when stable-idents (aset (.-env js/process) "LOGSEQ_STABLE_IDENTS" stable-idents)))))))

(defn- error-text [e]
  (let [s (string/join "\n" (take 12 (distinct (string/split-lines (str (or (.-stack e) e))))))]
    (subs s 0 (min 3000 (count s)))))

(defn- <mirror-pages
  "Writes every page as <mirror-repo! does (<mirror-page! on each named page;
  it skips the pages the mirror excludes), one page at a time so a page that
  fails to render costs only that page. Returns page db id -> error text."
  [db files]
  (p/loop [ids (sort (map :e (d/datoms db :avet :block/name))) crashes {}]
    (if-let [id (first ids)]
      (p/let [result (-> (p/resolved nil)
                         ;; in a then, so a throw while rendering rejects instead of escaping
                         (p/then (fn [_] (markdown-mirror/<mirror-page! test-repo db id {:platform (fake-platform files)})))
                         (p/catch (fn [e] {::crash (error-text e)})))]
        (p/recur (rest ids) (cond-> crashes (::crash result) (assoc id (::crash result)))))
      crashes)))

(defn- <kind-results [kind]
  (let [pages (vec (for [seed (range fuzz-seed (+ fuzz-seed fuzz-n))] (gen-page kind seed)))
        src-conn (try (db-test/create-conn-with-blocks (source-options pages)) (catch :default e e))]
    (if (instance? js/Error src-conn)
      (p/resolved [{:page (first pages) :mismatches [{:mismatch :crash/build-source :message (ex-message src-conn)}]}])
      (let [files (atom {})
            prefix (str (markdown-mirror/repo-mirror-dir test-repo) "/")
            src-db @src-conn]
        (-> (p/let [crashes (<mirror-pages src-db files)
                    mirror-files (into {} (map (fn [[k v]] [(string/replace-first k prefix "") v])) @files)
                    {:keys [conn notes]} (<import-mirror mirror-files)]
              (let [dst-db @conn
                    page->text (fn [page]
                                 (some (fn [[path text]]
                                         (when (string/includes? text (str "id:: " (:block/uuid page))) [path text]))
                                       mirror-files))]
                (concat
                 ;; an import error names its file: attach that file's content and its source page
                 (for [n notes]
                   (let [path (second (re-find #"Import failed on \"([^\"]+)\"" n))
                         text (get mirror-files path)
                         page (or (some (fn [page]
                                          (when-let [src-page (find-page src-db (:page page))]
                                            (when (and text (string/includes? text (str "id:: " (:block/uuid src-page)))) page)))
                                        pages)
                                  (first pages))]
                     {:page page
                      :text (when text [path text])
                      :mismatches [{:mismatch :import/error :message n}]}))
                      (for [page pages]
                        (let [src-page (find-page src-db (:page page))
                              dst-page (find-page dst-db (:page page))]
                          {:page page
                           :text (page->text src-page)
                           :mismatches (cond
                                         (get crashes (:db/id src-page))
                                         [{:mismatch :crash/mirror :message (get crashes (:db/id src-page))}]
                                         (not dst-page)
                                         [{:mismatch :page/missing}]
                                         :else
                                         (compare-views (cons (page-view src-db src-page) (tree-views src-db src-page 0))
                                                        (cons (page-view dst-db dst-page) (tree-views dst-db dst-page 0))))})))))
            (p/then vec)
            (p/catch (fn [e]
                       [{:page (first pages)
                         :mismatches [{:mismatch :crash/round-trip :message (error-text e)}]}])))))))

(defn- example-size [{:keys [text page]}] (count (or (second text) (pr-str (:blocks page)))))

(deftest mirror-then-import-fuzz
  (async done
         (let [kinds' (env-keywords "FUZZ_KINDS" kinds)
               started (js/Date.now)]
           (-> (p/loop [ks kinds' found {}]
                 (if-let [kind (first ks)]
                   (p/let [_ (when (= kind (first kinds')) (println "SCOPE not generated (asked upstream, FUZZ_ALL=1 to include):" (pr-str (when-not fuzz-all? (sort asked-upstream)))))
                _ (println "FUZZ kind" (name kind) "at ms" (- (js/Date.now) started))
                           results (<kind-results kind)
                           found' (reduce
                                   (fn [m {:keys [mismatches] :as result}]
                                     (reduce (fn [m {:keys [mismatch] :as x}]
                                               (let [k [kind mismatch]
                                                     ex (merge (select-keys result [:page :text]) x)
                                                     prev (get-in m [k :example])]
                                                 (-> m
                                                     (update-in [k :count] (fnil inc 0))
                                                     ;; every distinct file per kind, so one small example cannot hide others
                                                     (cond-> (:text ex) (update-in [k :files] (fnil conj #{}) [(:text ex) (:message x)]))
                                                     (assoc-in [k :example]
                                                               (if (and prev (<= (example-size prev) (example-size ex))) prev ex)))))
                                             m mismatches))
                                   found results)]
                     (p/recur (rest ks) found'))
                   (do
                     (println "FUZZ kinds" (count kinds') "seeds per kind" fuzz-n "from" fuzz-seed
                              "mismatches" (count found) "ms" (- (js/Date.now) started))
                     (println "ACCEPTED (rules for recorded not-bugs):" (pr-str (into (sorted-map) @accepted)))
                     (doseq [[[kind mismatch] {n :count example :example files :files}] (sort-by (comp str key) found)]
                       (println "\nKIND" (name kind) "MISMATCH" mismatch "count" n)
                       (println "  input:  " (pr-str (select-keys (:page example) [:page :blocks])))
                       (when (:message example) (println "  message:" (:message example)))
                       (when-let [[path text] (:text example)] (println "  mirror: " path (pr-str text)))
                       (when (:src example) (println "  before: " (pr-str (:src example))))
                       (when (:dst example) (println "  after:  " (pr-str (:dst example))))
                       (when (= :import/error mismatch)
                         (doseq [[[path text] message] (sort-by (comp str first) files)]
                           (println "  FILE" path (pr-str text))
                           (when message (println "    error:" (first (string/split-lines (str message))))))))
                     (is (empty? (sort (map str (keys found)))) "mirror then import gives back the same pages and blocks"))))
               (p/catch (fn [e] (is false (str "fuzz crashed: " e " " (subs (str (.-stack e)) 0 1500)))))
               (p/finally done)))))
