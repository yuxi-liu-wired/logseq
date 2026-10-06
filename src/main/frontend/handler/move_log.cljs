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
  "Where the block is: its parent row, its place among all rows of the page
  in screen order, and its siblings on screen (3 before, 3 after, the block
  in [ ]). A move changes the place or the parent even when the block is an
  only child before and after."
  [block-uuid]
  (if-let [^js node (block-node block-uuid)]
    (let [sibs (->> (array-seq (.-children (.-parentElement node)))
                    (filter #(and (.contains (.-classList %) "ls-block")
                                  (not (.contains (.-classList %) "block-add-button"))))
                    vec)
          i (.indexOf (clj->js sibs) node)
          shown (subvec sibs (max 0 (- i 3)) (min (count sibs) (+ i 4)))
          ^js parent (some-> (.-parentElement node) (.closest ".ls-block"))
          parent-row (when (and parent (not (.querySelector parent ":scope > .is-page-title-row")))
                       (row-text parent))
          ^js root (or (.closest node ".page-blocks-inner") (.closest node ".blocks-container") js/document)
          rows (array-seq (.querySelectorAll root ".ls-block:not(.block-add-button)"))
          place (.indexOf (clj->js (vec rows)) node)]
      {:where (str place "/" (some-> parent (.getAttribute "blockid")))
       :text (str (when parent-row (str "(in " parent-row ") "))
                  (string/join " | " (map #(if (identical? % node)
                                             (str "[" (row-text %) "]")
                                             (row-text %))
                                          shown)))})
    {:where nil :text "(block not on screen)"}))

(defn- row-sibs
  [^js node]
  (when-let [^js p (some-> node .-parentElement)]
    (->> (array-seq (.-children p))
         (filter #(and (.contains (.-classList %) "ls-block")
                       (not (.contains (.-classList %) "block-add-button"))))
         vec)))

(defn- can-move?
  "Whether the move has somewhere to go, as the outliner decides it: a
  sibling that way, or else a sibling of the parent row that way (1 level
  out only)."
  [block-uuid up?]
  (when-let [^js node (block-node block-uuid)]
    (let [sibs (row-sibs node)
          i (.indexOf (clj->js sibs) node)
          ^js parent (some-> (.-parentElement node) (.closest ".ls-block"))
          parent (when (and parent (not (.querySelector parent ":scope > .is-page-title-row"))) parent)
          psibs (when parent (row-sibs parent))
          pi (when parent (.indexOf (clj->js psibs) parent))]
      (if up?
        (or (pos? i) (and parent (pos? pi)))
        (or (< i (dec (count sibs))) (and parent (< pi (dec (count psibs)))))))))

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
  ;; a browser window only (not the unit tests' node)
  (when (and (exists? js/window) (fn? (.-addEventListener js/window))
             (not (.-__moveLogKeys js/window)))
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
        ;; the rows the move can carry: a selected page title never moves
        movable (->> (array-seq (js/document.querySelectorAll ".ls-block.selected"))
                     (remove #(.querySelector % ":scope > .is-page-title-row"))
                     ;; a selected row inside another selected row moves with
                     ;; it: only the top-level rows are moved, so only they
                     ;; say whether the move has somewhere to go
                     (remove #(some-> (.-parentElement %) (.closest ".ls-block.selected")))
                     (keep #(.getAttribute % "blockid")))
        block-uuid (or (some-> edit-block :block/uuid str) (first movable) (some-> (first selected) str))]
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
     :up? up?
     ;; a selection moves as 1 piece: down has somewhere to go if its last
     ;; row (page order) does, up if its first row does
     :edge-block (or (if up? (first movable) (last movable)) block-uuid)
     :before (around block-uuid)
     :steps (atom [])}))

(defn step!
  "Records a step of the move (what ran, what it returned)."
  [record what]
  (when record
    (swap! (:steps record) conj (str (js/Math.round (- (js/performance.now) (:t0 record))) "ms " what))))

(defn finish!
  "Where the block is once the move is drawn: the UI draws a move on the
  frame after it applies, so the second frame from now. Writes the line."
  [record]
  (when record
    (js/requestAnimationFrame
     #(js/requestAnimationFrame
     (fn []
       (let [after (around (:block record))
             before (:before record)
             moved? (and (some? (:where after)) (some? (:where before))
                         (not= (:where after) (:where before)))
             ;; a press with nowhere to go (first line up, last line down)
             ;; is an EDGE, not a miss. Judged on the page as it is now:
             ;; rows a delete or a move before this one took away are gone
             ;; by then (this move ran after them)
             can-move (can-move? (:edge-block record) (:up? record))
             verdict (cond moved? "moved" can-move "MISS" :else "EDGE")
             line (str (:at record) " " (:dir record) " "
                       verdict " key=" (:key record)
                       " " (:mode record) " block=" (:block record)
                       " steps=[" (string/join "; " @(:steps record)) "]"
                       " before=" (:text before) " after=" (:text after))]
         (write! line)
         ;; a miss shows on screen as it happens
         (when (= verdict "MISS")
           (notification/show! (str "Move " (string/lower-case (:dir record)) " did not move the block. Logged in ~/.logseq/move-log/") :warning true nil 4000))))))))
