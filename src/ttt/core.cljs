(ns ttt.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def empty-board [[:e :e :e]
                  [:e :e :e]
                  [:e :e :e]])

(def sym->char {:e "_"
            :x "X"
            :o "O"})

(def app-state (atom {:board empty-board}))

(defn clear-board! []
  (swap! app-state assoc :board empty-board))

(defn next-players-symbol [board]
  (if (odd? (count (filter #(= :e %) (apply concat board))))
    :x
    :o))

(defn check-win-rows [board sym]
  (some identity (map (fn [row] (every? #(= sym %) row)) board)))

(defn pivot [board]
  (map (fn [idx] (map #(nth % idx) board)) (range 3)))

(defn check-win-cols [board sym]
  (check-win-rows (pivot board) sym))

(defn check-win-diag [board sym]
  (let [d1 (map #(get-in board [% %]) (range 3))
        d2 (map #(get-in board [% (- 2 %)]) (range 3))]
    (or (every? #(= sym %) d1)
        (every? #(= sym %) d2))))

(defn check-win-state-for-player [board sym]
  (or (check-win-rows board sym)
      (check-win-cols board sym)
      (check-win-diag board sym)))

(defn check-win-state [board]
  (cond
   (check-win-state-for-player board :x)
   :x

   (check-win-state-for-player board :o)
   :o

   :else
   nil))

(defn drawn? [board]
  (and (= 0 (count (filter #(= :e %) (apply concat board))))
       (nil? (check-win-state board))))

(defn board-is-active? [board]
  (and (not (drawn? board))
       (nil? (check-win-state board))))

(defn update-board! [r c s]
  (swap! app-state assoc-in [:board r c] s))

(defn cell-is-empty? [board r c]
  (= :e (get-in board [r c])))

(defn tic-tac-toe-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {})
    om/IRenderState
    (render-state [this state]
      (dom/div
       nil
       (cond
        (board-is-active? (:board app))
        (dom/div #js {:className "winner"}
                 "Nobody has won yet - keep playing!!")

        (drawn? (:board app))
        (dom/div #js {:className "drawn"}
                 "The Game is a Draw!")

        :else
        (dom/div #js {:className "won"}
                 "The Winner: " (get sym->char (check-win-state (:board app)))))

       (dom/button #js {:onClick (fn [e] (clear-board!))} "Reset")
       (apply dom/table
              nil

              (map-indexed (fn [row-idx row]
                             (apply dom/tr
                                    nil
                                    (map-indexed
                                     (fn [col-idx col]
                                       (dom/td #js {:onClick (fn [e]
                                                               (if (and
                                                                    (cell-is-empty? (:board @app) row-idx col-idx)
                                                                    (board-is-active? (:board @app)))
                                                                 (update-board! row-idx col-idx (next-players-symbol (:board @app)))))}
                                               (get sym->char col))) row))) (:board app)))))))

(om/root
 tic-tac-toe-view
 app-state
 {:target (. js/document (getElementById "app"))})
