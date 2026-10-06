(ns frontend.handler.move-log
  "Fork-only record of every move-up / move-down press, for finding presses
  that do not move the block. 1 line per press, appended to
  ~/.logseq/move-log/<day>.log by the desktop app. A press whose block ends
  where it started is marked MISS."
  (:require [clojure.string :as string]
            [electron.ipc :as ipc]
            [frontend.handler.notification :as notification]
            [frontend.state :as state]
            [frontend.util :as util]))

(defn- row-text
  [^js node]
  (let [t (some-> node (.querySelector ":scope > .block-main-container textarea"))
        c (some-> node (.querySelector ":scope > .block-main-container .block-content"))
        s (string/trim (or (if t (.-value t) (some-> c .-textContent)) ""))]
    (if (string/blank? s) "_" (subs s 0 (min 24 (count s))))))

(defn- block-node
  [block-uuid]
  (when block-uuid
    (js/document.querySelector (str ".ls-block[blockid=\"" block-uuid "\"]"))))

(defn- around
  "The block's siblings on screen, 3 before and 3 after, the block in [ ]."
  [block-uuid]
  (if-let [^js node (block-node block-uuid)]
    (let [sibs (->> (array-seq (.-children (.-parentElement node)))
                    (filter #(and (.contains (.-classList %) "ls-block")
                                  (not (.contains (.-classList %) "block-add-button"))))
                    vec)
          i (.indexOf (clj->js sibs) node)
          shown (subvec sibs (max 0 (- i 3)) (min (count sibs) (+ i 4)))]
      {:index i
       :text (string/join " | " (map #(if (identical? % node)
                                         (str "[" (row-text %) "]")
                                         (row-text %))
                                      shown))})
    {:index nil :text "(block not on screen)"}))

(defn- write!
  [line]
  (js/console.info "[move-log]" line)
  (when (util/electron?)
    (-> (ipc/ipc :appendMoveLog line)
        (.catch (fn [e] (js/console.error "[move-log] write failed" e))))))

(defonce ^:private *last-run (atom 0))

(defn- install-key-watch!
  "Every keydown of an arrow with Shift or Alt held, as the window gets it:
  if no move ran for it within 300 ms, it is logged as UNHANDLED (the key
  reached the app; no shortcut took it)."
  []
  (when-not (.-__moveLogKeys js/window)
    (set! (.-__moveLogKeys js/window) true)
    (.addEventListener
     js/window "keydown"
     (fn [^js e]
       (when (and (contains? #{"ArrowUp" "ArrowDown"} (.-key e))
                  (or (.-shiftKey e) (.-altKey e) (.-metaKey e)))
         (let [t (js/performance.now)
               k (str (when (.-ctrlKey e) "ctrl+") (when (.-metaKey e) "cmd+")
                      (when (.-altKey e) "alt+") (when (.-shiftKey e) "shift+")
                      (.-key e) (when (.-repeat e) " (held)"))
               target (let [^js tg (.-target e)] (str (some-> tg .-tagName string/lower-case) (when (seq (.-id tg)) (str "#" (.-id tg)))))]
           (js/setTimeout
            (fn []
              ;; a move press with Shift and Alt (or Cmd) held that ran no move
              (when (and (.-shiftKey e) (or (.-altKey e) (.-metaKey e))
                         (< @*last-run t))
                (write! (str (.toISOString (js/Date.)) " UNHANDLED key=" k " target=" target
                             " defaultPrevented=" (.-defaultPrevented e)))
                (notification/show! (str "Move key " k " reached the app but no move ran. Logged in ~/.logseq/move-log/") :warning true nil 4000)))
            300))))
     true)))

(install-key-watch!)

(defn start
  "Called on every move press, before anything else runs. Returns the
  record that `finish!` completes."
  [up? ^js event]
  (let [edit-block (state/get-edit-block)
        selected (state/get-selection-block-ids)
        block-uuid (or (some-> edit-block :block/uuid str) (some-> (first selected) str))]
    (reset! *last-run (js/performance.now))
    {:t0 (js/performance.now)
     :at (.toISOString (js/Date.))
     :dir (if up? "UP" "DOWN")
     ;; the shortcut handler passes its own event; the browser's key event
     ;; is inside it
     :key (let [^js e (or (some-> event .-browserEvent) event)]
            (when e
              (str (when (.-ctrlKey e) "ctrl+") (when (.-metaKey e) "cmd+")
                   (when (.-altKey e) "alt+") (when (.-shiftKey e) "shift+")
                   (or (.-key e) (.-identifier event)) (when (.-repeat e) " (held)"))))
     :mode (cond edit-block "editing" (seq selected) (str "selected " (count selected)) :else "nothing")
     :block block-uuid
     :before (around block-uuid)
     :steps (atom [])}))

(defn step!
  "Records a step of the move (what ran, what it returned)."
  [record what]
  (when record
    (swap! (:steps record) conj (str (js/Math.round (- (js/performance.now) (:t0 record))) "ms " what))))

(defn finish!
  "1 frame after the move settles: where the block is now. Writes the line."
  [record]
  (when record
    (js/setTimeout
     (fn []
       (let [after (around (:block record))
             before (:before record)
             moved? (and (some? (:index after)) (some? (:index before))
                         (not= (:text after) (:text before)))
             line (str (:at record) " " (:dir record) " "
                       (if moved? "moved" "MISS") " key=" (:key record)
                       " " (:mode record) " block=" (:block record)
                       " steps=[" (string/join "; " @(:steps record)) "]"
                       " before=" (:text before) " after=" (:text after))]
         (write! line)
         ;; a miss shows on screen as it happens
         (when-not moved?
           (notification/show! (str "Move " (string/lower-case (:dir record)) " did not move the block. Logged in ~/.logseq/move-log/") :warning true nil 4000))))
     600)))
