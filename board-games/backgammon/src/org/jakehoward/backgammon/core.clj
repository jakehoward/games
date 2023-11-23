(ns org.jakehoward.backgammon.core
  (:require [org.jakehoward.backgammon.utils :refer [roll-die remove-one]]))

(defrecord Man [player id])

;; Less messy and space consuming to look at board
;; if Man prints as a map
(defmethod print-method Man [^Man inst ^java.io.Writer w]
  (print-method (into {} inst) w))

(defn make-man [player id-atom]
  (if (#{:p1 :p2} player)
    (->Man player (swap! id-atom inc))
    (throw (Exception. (str "Valid players are :p1 and :p2, not:" player)))))

(defn p1-man? [man] (= :p1 (:player man)))
(defn p2-man? [man] (= :p2 (:player man)))

(def men-per-player 15)

(defn finished? [board]
  (->> (:borne-off board)
       (group-by p1-man?)
       vals
       (some #(= (count %) men-per-player))
       boolean))

(def legal-first-rolls
  (for [d1 (range 1 7)
        d2 (range 1 7)
        :when (not= d1 d2)]
    [d1 d2]))

(def empty-point->men (into {} (map (fn [i] [i []]) (range 1 25))))

;; assertions
;; - men-per-player matches board totals
(def initial-setup
  (let [men-id    (atom 0)
        p1 (fn [] (make-man :p1 men-id))
        p2 (fn [] (make-man :p2 men-id))]
    {:p1-direction :desc
     :point->men   (merge
                    empty-point->men
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

(defrecord Move [from to])

(defn get-legal-moves [player-id {:keys [board die-rolls is-p1-turn]}])

(defn random-moves [player-id ctx]
  (let [legal-moves (get-legal-moves player-id ctx)]
    (rand-nth (into [] legal-moves))))

(defprotocol Player
  (choose-moves [this ctx]))

(defrecord NaivePlayer [player-id]
  Player
  (choose-moves [this ctx] (random-moves player-id ctx)))

(defn apply-move [board {:keys [from to] :as move}]
  (let [is-re-entry     (#{:p1-bar :p2-bar} from)
        re-entry-player ({:p1-bar :p1, :p2-bar :p2} from)
        man             (if is-re-entry
                          (->> (:bar board)
                               (drop-while #(not= re-entry-player (:player %)))
                               first)
                          (-> board (get-in [:point->men from]) peek))
        curr-to     (get-in board [:point->men to])
        is-bear-off (= to :borne-off)
        is-take     (and (seq curr-to)
                         (not= (:player man)
                               (-> curr-to peek :player)))
        new-from    (-> board (get-in [:point->men from]) pop)
        new-to      (if is-take
                      [man]
                      (conj curr-to man))
        new-bar     (if is-take
                      (conj (:bar board) (-> board (get-in [:point->men to]) peek))
                      (:bar board))
        new-bar     (if is-re-entry
                      (remove-one #(= man %) new-bar)
                      new-bar)
        new-p->m    (-> (:point->men board)
                        ((fn [b] (if is-re-entry b (assoc b from new-from))))
                        ((fn [p->m] (if is-bear-off p->m (assoc p->m to new-to)))))
        new-bo      (-> (if is-bear-off
                          (conj (:borne-off board) man)
                          (:borne-off board)))]
    (-> board
        (assoc :point->men new-p->m)
        (assoc :bar new-bar)
        (assoc :borne-off new-bo))))

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
           ctxs         []
           legal-move   true]

      (if (or (not legal-move)
              (>= n max-iterations)
              (finished? board))

        {:iterations n
         :ctxs       ctxs
         :finished   (finished? board)}

        (let [player      (if is-p1-turn p1 p2)
              ctx         {:board board :die-rolls die-rolls :p1 is-p1-turn}
              moves       (choose-moves player ctx)
              legal-moves (get-legal-moves (:player-id player) ctx)
              next-board  (reduce apply-move board moves)]
          (recur (inc n)
                 [(roll-die) (roll-die)]
                 (not is-p1-turn)
                 next-board
                 (conj ctxs ctx)
                 (contains? legal-moves moves)))))))

(comment
  initial-setup
  (as-> (play (->NaivePlayer :p1) (->NaivePlayer :p2)) $
    (:ctxs $)
    (map :p1 $))

  (contains? #{[(->Move 1 2) (->Move 3 4)]} [(->Move 1 2) (->Move 3 5)])
  ;
  )
