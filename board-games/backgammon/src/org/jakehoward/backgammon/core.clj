(ns org.jakehoward.backgammon.core
  (:require [org.jakehoward.backgammon.utils :refer [roll-die num-die-sides]]
            [clojure.core :refer [abs]]
            [malli.core :as m]
            [org.jakehoward.backgammon.schema :as s]
            [clojure.set :as set]
            [org.jakehoward.backgammon.constants :as c]
            [clojure.math.combinatorics :as combo]))

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

(def legal-first-rolls
  (for [d1 (range 1 7)
        d2 (range 1 7)
        :when (not= d1 d2)]
    [d1 d2]))

(def empty-point->men (into {:bar {:p1 [] :p2 []} :borne-off []}
                            (map (fn [i] [i []]) (range 1 25))))

(defn player-generator []
  (let [id-atom (atom 0)]
    {:p1 (fn [] (make-man :p1 id-atom))
     :p2 (fn [] (make-man :p2 id-atom))}))

;; assertions
;; - men-per-player matches board totals
(def initial-setup
  (let [{:keys [p1 p2]} (player-generator)]
    {:point->men   (merge
                    empty-point->men
                    {1          (vec (repeatedly 2 p2))
                     6          (vec (repeatedly 5 p1))
                     8          (vec (repeatedly 3 p1))
                     12         (vec (repeatedly 5 p2))
                     13         (vec (repeatedly 5 p1))
                     17         (vec (repeatedly 3 p2))
                     19         (vec (repeatedly 5 p2))
                     24         (vec (repeatedly 2 p1))})}))

;; todo - memoize me, measure perf before/after
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

;; =====
;; Rules
;; =====
(defrecord Move [from to])
(defn move? [x] (= org.jakehoward.backgammon.core.Move (type x)))
(defn valid-move? [{:keys [from to] :as move}]
  (and
   (m/validate s/Move move)
   (not= from to)
   (if (and (int? from) (int? to)) (<= (abs (- from to)) num-die-sides) true)))

(defn is-set-of-valid-moves? [poss-set-of-moves]
  (and
   (set? poss-set-of-moves)
   (if (seq poss-set-of-moves)
     (every? #(= (count %) (count (first poss-set-of-moves))) poss-set-of-moves)
     true)
   (every? (fn [poss-moves] (and (vector? poss-moves)
                                 (every? move? poss-moves)
                                 (every? valid-move? poss-moves)))
           poss-set-of-moves)))

(defn home-board? [player pos]
  (if (= :p1 player)
    (< pos 7)
    (> pos 18)))

(defn point-occupied-by? [board point player]
  (-> (get-in board [:point->men point])
      first
      :player
      (= player)))

(comment
  (let [{:keys [p1 p2]} (player-generator)
        board           {:point->men (merge empty-point->men
                                            {2 [(p1)]
                                             3 [(p1) (p1)]
                                             4 [(p2)]
                                             5 [(p2) (p2)]})}]
    (->>  [2 3 4 5]
          (map (fn [point] ((juxt #(point-occupied-by? board % :p1)
                                  #(point-occupied-by? board % :p2))
                            point))))))

(defn on-the-bar? [board player]
  (-> board
      (get-in [:point->men :bar player])
      seq
      boolean))

(comment
  (let [{:keys [p1 p2]} (player-generator)
        board           {:point->men (merge empty-point->men
                                            {:bar {:p1 [(p1)] :p2 []}})}]
    ((juxt #(on-the-bar? board :p1)
           #(on-the-bar? board :p2)))))

(defn can-bear-off? [board player]
  (and (not (on-the-bar? board player))
       (->> (keys (:point->men board))
            (filter integer?)
            (filter #(point-occupied-by? board % player))
            (every? #(home-board? player %)))))

(comment
  (let [{:keys [p1 p2]} (player-generator)
        board           {:point->men (merge empty-point->men
                                            {2  [(p1)]
                                             ;; :bar {:p1 [(p1)] :p2 [(p2)]}
                                             3  [(p1) (p1)]
                                             ;; 12 [(p2)]
                                             22 [(p2) (p2)]})}]
    ((juxt #(can-bear-off? board :p1)
           #(can-bear-off? board :p2)))))

(defn can-land-on-point? [board player point]
  (-> board
      (get-in [:point->men point])
      ((some-fn empty?
                #(= 1 (count %))
                #(= player (:player (peek %)))))))

(comment
  (let [{:keys [p1 p2]} (player-generator)
        board           {:point->men (merge empty-point->men
                                            {1  []
                                             2  [(p1)]
                                             3  [(p1) (p1)]
                                             4  [(p2)]
                                             5  [(p2) (p2)]})}]
    (->> [1 2 3 4 5]
         (map (fn [point] ((juxt #(can-land-on-point? board :p1 %)
                                 #(can-land-on-point? board :p2 %))
                           point))))))

(defn player-occupies-point? [board player point]
  (= player (-> board (get-in [:point->men point]) peek :player)))

(defn get-point [board point]
  (get-in board [:point->men point]))

(defn get-legal-moves-1 [{:keys [board die-roll player]}]
  (let [standard-moves
        (fn []
          (let [froms          (->> (range 1 25)
                                    (filter (partial player-occupies-point? board player) ))
                from-to-pairs  (->> froms
                                    (map (fn [from] (if (= :p1 player)
                                                      [from (- from die-roll)]
                                                      [from (+ from die-roll)])))
                                    (filter (fn [[from to]]
                                              (and (> from 0)
                                                   (> to 0)
                                                   (< from 25)
                                                   (< from 25)
                                                   (can-land-on-point? board player to)))))
                moves           (map (fn [[f t]] (->Move f t)) from-to-pairs)]
            (set moves)))]

    (cond
      (on-the-bar? board player)
      (let [point (if (= :p1 player) (- 25 die-roll) die-roll)]
        (if (can-land-on-point? board player point)
          #{(->Move [:bar player] point)}
          #{}))

      ;; You MUST bear off the highest man you can
      (can-bear-off? board player)
      (let [bear-off-moves
            (reduce (fn [moves point]
                      (if (and (empty? moves)
                               (= player (-> board (get-in [:point->men point]) peek :player))
                               (>= die-roll (if (= :p1 player) point (- 25 point))))
                        (conj moves (->Move point :borne-off))
                        moves))
                    #{}
                    (if (= :p1 player) (range 6 0 -1) (range 19 25)))]
        (if (seq bear-off-moves)
          bear-off-moves
          (standard-moves)))

      :else
      (standard-moves))))

(defn get-legal-moves
  [{:keys [board die-rolls player]}]

  {:pre  [(m/validate s/Player player)
          (m/validate s/Board board)
          (m/validate s/DieRolls die-rolls)]
   :post [(m/validate s/LegalMoves %) (is-set-of-valid-moves? %)]}

  ;; You must maximise the utilisation of the die rolls
  ;; - If you can only move one or the other of the die rolls,
  ;;   it MUST be the larger of the two

  ;; If you have men on the bar you MUST move them first

  ;; If die rolls are the same number, you get to move 4 men instead of 2
  ;; - you can move the same man 4 times (assuming legal moves)

  ;; Move from current position to:
  ;; - Empty point
  ;; - Point containing your men
  ;; - Point with 1 enemy man

  ;; You can bear off if:
  ;; - you don't have any men on the bar
  ;; - and all your men are on your home board
  ;; - man exists on point_num <= die_num (normalised for p2)

  ;; You can't use all die rolls if you don't have enough men to do so
  ;; - e.g. last item borne off

  ;; Don't forget that each move can change the board in such a way
  ;; as to make subsequent moves un/available

  ;; If n moves aren't moving the same man, they can be played in any order
  ;; - allowing this makes it more robust to plugging in other bots and allowing
  ;;   human gameplay whilst still being able to check the legality of moves

  (let [[d1 d2]         die-rolls
        all-rolls       (if (= d1 d2)
                          (-> (repeat 2 die-rolls) flatten vec)
                          die-rolls)
        all-roll-combos (combo/permutations all-rolls)]

    (letfn [(get-moves [board rolls]
              (if (empty? rolls)
                []
                (let [moves (get-legal-moves-1
                             {:board board :die-roll (first rolls) :player player})]
                  (->> moves
                       (mapcat (fn [m]
                                 (let [next-board (apply-move board m)
                                       next-move-vecs (get-moves next-board (rest rolls))]
                                   (if (seq next-move-vecs)
                                     (mapv (fn [nmv] (into [m] nmv)) next-move-vecs)
                                     [[m]]))))
                       (into [])))))]

      (let [all-valid-moves (->>  all-roll-combos
                                  (mapcat (partial get-moves board))
                                  set)]
        (if (and (not= d1 d2)
                 (seq all-valid-moves)
                 (= 1 (apply max (map count all-valid-moves))))
          ;; todo - choose the move with the biggest magnitude
          all-valid-moves
          all-valid-moves
          )
        ))))

(comment
  ;; different types...(sort in impl?)
  (combo/permutations [5 6])
  (combo/permutations [6 5])

  (reverse (sort [1 5 2 6]))
  (valid-move? (->Move [:bar :p1] 19))
  (get-legal-moves {}) ;; => no!
  (get-legal-moves {}))

;; ========
;; gameplay
;; ========
(defprotocol Player
  (choose-moves [this ctx]))

(defrecord NaivePlayer [player-id]
  Player
  (choose-moves [this ctx]
    (let [legal-moves (get-legal-moves (assoc ctx :player player-id))]
      (rand-nth (into [] legal-moves)))))

(defn finished? [board]
  (->> (get-in board [:point->men :borne-off])
       (group-by p1-man?)
       vals
       (some #(= (count %) c/men-per-player))
       boolean))

(defn play [p1 p2]
  (let [max-iterations   5
        initial-roll     (rand-nth legal-first-rolls)
        [p1-die p2-die]  initial-roll
        players          (if (> p1-die p2-die) [p1 p2] [p2 p1])]

    (loop [n            0
           die-rolls    initial-roll
           players      players
           board        initial-setup
           history      []
           legal-move   true]

      (if (or (not legal-move)
              (>= n max-iterations)
              (finished? board))

        {:iterations   n
         :history      (conj history {:board board :die-rolls die-rolls})
         :illegal-move (not legal-move)
         :finished     (finished? board)}

        (let [player         (first players)
              ctx            {:board board :die-rolls die-rolls :player (:id player)}
              moves          (choose-moves player ctx)
              history-item   (assoc ctx :moves moves)
              legal-moves    (get-legal-moves ctx)
              next-board     (reduce apply-move board moves)]
          (recur (inc n)
                 [(roll-die) (roll-die)]
                 (reverse players)
                 next-board
                 (conj history history-item)
                 (contains? legal-moves moves)))))))

(comment

  (defrecord NoOpPlayer [id]
    Player
    (choose-moves [this ctx] []))

  (play (->NoOpPlayer :p1) (->NoOpPlayer :p2))

  (reverse (reverse [:a :b]))
  initial-setup
  (as-> (play (->NaivePlayer :p1) (->NaivePlayer :p2)) $
    (:ctxs $)
    (map :p1 $))

  (contains? #{[(->Move 1 2) (->Move 3 4)]} [(->Move 1 2) (->Move 3 4)])
  ;
  )
