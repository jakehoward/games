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


(defn play [p1 p2]
  (let [max-iterations   5
        initial-roll     (rand-nth legal-first-rolls)
        [p1-die p2-die]  initial-roll
        ;; needs primitive not Boolean??
        is-p1-turn       (if (> p1-die p2-die) true false)]

    (loop [n            0
           die-rolls    initial-roll
           is-p1-turn   is-p1-turn
           board        initial-setup
           ctxs         []]

      (if (or (>= n max-iterations)
              (finished? board))

        {:iterations n
         :ctxs       ctxs
         :finished   (finished? board)}

        (let [player      (if is-p1-turn p1 p2)
              ctx         {:board board :die-rolls die-rolls :p1 is-p1-turn}
              next-board  (take-turn player ctx)]
          (recur (inc n)
                 [(roll-die) (roll-die)]
                 (not is-p1-turn)
                 next-board
                 (conj ctxs ctx)))))))


(comment
  initial-setup
  (as-> (play (->NaivePlayer p1?) (->NaivePlayer p2?)) $
    (:ctxs $)
    (map :p1 $))
  ;
  )
