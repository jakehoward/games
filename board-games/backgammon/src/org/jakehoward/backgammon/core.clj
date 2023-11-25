(ns org.jakehoward.backgammon.core
  (:require [org.jakehoward.backgammon.utils :refer [roll-die num-die-sides remove-one]]
            [clojure.core :refer [abs]]))

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

(def empty-point->men (into {:bar {:p1 [] :p2 []} :borne-off []}
                            (map (fn [i] [i []]) (range 1 25))))

;; assertions
;; - men-per-player matches board totals
(def initial-setup
  (let [men-id    (atom 0)
        p1 (fn [] (make-man :p1 men-id))
        p2 (fn [] (make-man :p2 men-id))]
    {:p1-direction :desc
     :point->men   (merge
                    empty-point->men
                    {1          (vec (repeatedly 2 p2))
                     6          (vec (repeatedly 5 p1))
                     8          (vec (repeatedly 3 p1))
                     12         (vec (repeatedly 5 p2))
                     13         (vec (repeatedly 5 p1))
                     17         (vec (repeatedly 3 p2))
                     19         (vec (repeatedly 5 p2))
                     24         (vec (repeatedly 2 p1))})
     :bar         []
     :borne-off   []}))

;; =====
;; Rules
;; =====
(defrecord Move [from to])
(defn move? [x] (= org.jakehoward.backgammon.core.Move (type x)))
(defn valid-move? [{:keys [from to] :as move}]
  (and
   (move? move)
   (not= from to)
   (if (and (int? from) (int? to)) (<= (abs (- from to)) num-die-sides) true)
   (cond (= [:bar :p1] from) (< (- 24 to) num-die-sides)
         (= [:bar :p2] from) (<= to num-die-sides)
         :else               true)
   (contains? (into #{:borne-off} (range 1 25)) to)
   (contains? (into #{[:bar :p1] [:bar :p2]} (range 1 25)) from)))

(defn is-set-of-valid-moves? [poss-set-of-moves]
  (and
   (set? poss-set-of-moves)
   (every? (fn [poss-moves] (and (vector? poss-moves)
                                 (every? move? poss-moves)
                                 (every? valid-move? poss-moves)))
           poss-set-of-moves)))

(defn get-legal-moves
  [player-id {:keys [board die-rolls is-p1-turn]}]
  {:pre  [(#{:p1 :p2} player-id)]
   :post [(is-set-of-valid-moves? %)]}
  #{})

(comment
  (valid-move? (->Move [:bar :p1] 19))
  (get-legal-moves :p3 {}) ;; => no!
  (get-legal-moves :p1 {})
  )

;; ========
;; gameplay
;; ========
(defprotocol Player
  (choose-moves [this ctx]))

(defrecord NaivePlayer [player-id]
  Player
  (choose-moves [this ctx]
    (let [legal-moves (get-legal-moves player-id ctx)]
      (rand-nth (into [] legal-moves)))))

(defn apply-move [board {:keys [from to] :as move}]
  (let [from-path   (flatten (into [:point->men] (vector from)))
        to-path     (flatten (into [:point->men] (vector to)))
        man         (-> board (get-in from-path) peek)
        curr-to     (get-in board to-path)
        is-take     (and (seq curr-to)
                         (not= (:player man)
                               (-> curr-to peek :player)))
        new-from    (-> board (get-in from-path) pop)
        new-to      (if is-take
                      [man]
                      (conj curr-to man))
        new-board    (-> board
                         (assoc-in from-path new-from)
                         (assoc-in to-path new-to)
                         ((fn [b] (if-not is-take
                                    b
                                    (let [taken-man (peek curr-to)]
                                      (update-in b
                                                 [:point->men :bar (:player taken-man)]
                                                 (fn [c] (conj c taken-man))))))))]
    new-board))

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
