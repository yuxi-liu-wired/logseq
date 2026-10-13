(ns frontend.worker.markdown-mirror-import-probe-test
  "One-block cases for the mismatch kinds of markdown-mirror-import-fuzz-test:
  a mirror crash, and Markdown files imported by the file-graph importer."
  (:require [cljs.test :refer [async deftest is]]
            [clojure.string :as string]
            [datascript.core :as d]
            [frontend.worker.markdown-mirror :as markdown-mirror]
            [logseq.db.frontend.content :as db-content]
            [logseq.db.test.helper :as db-test]
            [logseq.graph-parser.exporter :as gp-exporter]
            [promesa.core :as p]))

(def ^:private test-repo "logseq_db_mirror-probe")

(defn- fake-platform [files]
  {:env {:runtime :node}
   :storage {:read-text! (fn [path] (p/resolved (get @files path)))
             :write-text-atomic! (fn [path content] (swap! files assoc path content) (p/resolved nil))
             :delete-file! (fn [path] (swap! files dissoc path) (p/resolved nil))}
   :broadcast {:post-message! (fn [& _] nil)}})

(defn- <import [files]
  (let [conn (db-test/create-conn)
        files (assoc files "logseq/config.edn" "{:file/name-format :triple-lowbar}")
        notes (atom [])]
    (-> (gp-exporter/export-file-graph
         conn conn {:path "logseq/config.edn"} (mapv #(hash-map :path %) (sort (keys files)))
         {:<read-file (fn [file] (p/resolved (get files (:path file))))
          :rpath-key :path
          :notify-user (fn [m] (when (= :error (:level m)) (swap! notes conj (str (:msg m)))))
          :user-options {:convert-all-tags? true :remove-inline-tags? true}
          :<read-and-copy-asset (fn [& _] (p/resolved nil))})
        (p/then (fn [_] {:conn conn :notes @notes}))
        (p/catch (fn [e] {:conn conn :notes (conj @notes (ex-message e))})))))

(defn- page-blocks [db page-name]
  (when-let [page (d/entity db (d/q '[:find ?p . :in $ ?n :where [?p :block/name ?n]] db page-name))]
    (->> (d/q '[:find [?b ...] :in $ ?p :where [?b :block/page ?p]] db (:db/id page))
         (map #(d/entity db %))
         (sort-by :block/order)
         (mapv (fn [b] {:title (db-content/recur-replace-uuid-in-block-title b)
                        :display-type (:logseq.property.node/display-type b)
                        :link (some-> (:block/link b) :block/title)
                        :tags (set (map :block/title (:block/tags b)))})))))

(def ^:private import-cases
  [["math after the page id line" "id:: 8698591b-fc0e-4fbb-8403-d524e640435a\n- $$\n  E = mc^2\n  $$"]
   ["math after a blank line" "id:: 8698591b-fc0e-4fbb-8403-d524e640435a\n\n- $$\n  E = mc^2\n  $$"]
   ["math, no page id" "- $$\n  E = mc^2\n  $$"]
   ["embed of an unknown uuid" "- a\n- b {{embed [[10307562-adaf-4557-89c4-bc8bbcd0d20e]]}}"]
   ["text before a page embed" "- Beta {{embed [[Alpha]]}}"]
   ["a ref to an unknown uuid" "- c [[989dffed-9ed7-4867-8d76-1c74b20a2595]]"]])

(deftest import-probe
  (async done
         (-> (p/loop [cases import-cases]
               (when-let [[label text] (first cases)]
                 (p/let [{:keys [conn notes]} (<import {"pages/probe.md" text "pages/Alpha.md" "- alpha"})]
                   (println "PROBE import" (pr-str label) (pr-str text))
                   (println "  notes: " (pr-str notes))
                   (println "  blocks:" (pr-str (page-blocks @conn "probe")))
                   (p/recur (rest cases)))))
             (p/finally (fn [] (is true) (done))))))

(defn- <mirror-probe [label blocks]
  (let [conn (db-test/create-conn-with-blocks
              {:classes {:research {}}
               :pages-and-blocks [{:page {:block/title "probe"} :blocks blocks}]})
        files (atom {})
        page-id (d/q '[:find ?p . :where [?p :block/name "probe"]] @conn)]
    (-> (p/resolved nil)
        ;; a repo per probe: the mirror caches per-repo state
        (p/then (fn [_] (markdown-mirror/<mirror-page! (str test-repo "-" (hash label)) @conn page-id {:platform (fake-platform files)})))
        (p/then (fn [_] (println "PROBE mirror:" label (pr-str (vals @files)))))
        (p/catch (fn [e] (println "PROBE mirror:" label "throws:" (first (string/split-lines (str e)))))))))

(deftest mirror-probe
  (async done
         (let [a #uuid "11111111-1111-4111-8111-111111111111"]
           (-> (<mirror-probe "a math block, then a task with a tag"
                              [{:block/title "E = mc^2"
                                :build/properties {:logseq.property.node/display-type :math}
                                :build/tags [:logseq.class/Math-block]}
                               {:block/title "after"
                                :build/properties {:logseq.property/status :logseq.property/status.todo}
                                :build/tags [:logseq.class/Task :research]}])
               (p/then (fn [_]
                         (<mirror-probe "embed of the parent"
                                        [{:block/title "A" :block/uuid a :build/keep-uuid? true
                                          :build/children [{:block/title "" :block/link [:block/uuid a]}]}])))
               (p/catch (fn [e] (println "PROBE mirror: error" (ex-message e))))
               (p/finally (fn [] (is true) (done)))))))
