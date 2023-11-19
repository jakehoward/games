(ns org.jakehoward.backgammon.core
  (:require [org.jakehoward.backgammon.utils :refer [roll-die]]))

(def p1? even?)
(def p2? odd?)

(def men-per-player 15)

(defn finished? [board]
  (->> (:borne-off board)
       (group-by p1?)
       vals
       (some #(= (count %) men-per-player))
       boolean))

(def legal-first-rolls
  (for [d1 (range 1 7)
        d2 (range 1 7)
        :when (not= d1 d2)]
    [d1 d2]))

;; assertions
;; - men-per-player matches board totals
(def initial-setup
  (let [p1-men-id (atom 0)
        p2-men-id (atom -1)
        p1 (fn [] (swap! p1-men-id #(+ % 2)))
        p2 (fn [] (swap! p2-men-id #(+ % 2)))]
    {:point->men (merge
                  (into {} (map (fn [i] [i []]) (range 1 25)))
                  {1  (vec (repeatedly 2 p2))
                   6  (vec (repeatedly 5 p1))
                   8  (vec (repeatedly 3 p1))
                   12 (vec (repeatedly 5 p2))
                   13 (vec (repeatedly 5 p1))
                   17 (vec (repeatedly 3 p2))
                   19 (vec (repeatedly 5 p2))
                   24 (vec (repeatedly 2 p1))})
     :bar         []
     :borne-off   []}))


(defn random-take-turn [my-man? {:keys [board die-rolls]}]
  board)


(defprotocol Player
  (take-turn [this ctx]))

(defrecord NaivePlayer [my-man?]
  Player
  (take-turn [this ctx] (random-take-turn my-man? ctx)))


(defn game [p1 p2]
  (let [max-iterations   5
        initial-roll     (rand-nth legal-first-rolls)
        [p1-die p2-die]  initial-roll]

    (loop [n                0
           die-rolls        initial-roll
           is-player-1-turn (if (> p1-die p2-die) true false) ;; needs primitive not Boolean??
           boards           [initial-setup]]

      (if (or (> n max-iterations)
              (finished? (last boards)))

        {:iterations n
         :boards boards
         :finished (finished? (last boards))}

        (let [player      (if is-player-1-turn p1 p2)
              ctx         {:board (last boards) :die-rolls die-rolls}
              next-board  (take-turn player ctx)]
          (recur (inc n)
                 [(roll-die) (roll-die)]
                 (not is-player-1-turn)
                 (conj boards next-board)))))))


(comment
  initial-setup
  (game (->NaivePlayer p1?) (->NaivePlayer p2?))
  ;
  )
