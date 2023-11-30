(ns org.jakehoward.backgammon.core-test
  (:require [org.jakehoward.backgammon.core :as bg]
            [clojure.set :as set]
            [malli.core :as m]
            [malli.error :as me]
            [org.jakehoward.backgammon.schema :as s]
            [org.jakehoward.backgammon.utils :as u]
            [clojure.test :as t]))

(defn empty-board-with [p->m]
  (-> bg/initial-setup
      (assoc :point->men (u/deep-merge bg/empty-point->men p->m))))

;; property based ideas
;; - if not bearning off, number of men on board invariant
;; - num men total is invariant
;; - :p1-direction is invariant in game
;; - if bearning off, number of men removed <= 2 unless double <= 4
;; - if not bearning off, all legal moves from - to = die roll, die roll only used once

(t/deftest test-get-legal-moves
  (t/testing "initial board"
    (let [board               bg/initial-setup

          legal-moves         (bg/get-legal-moves {:board board :die-rolls [1 2] :player :p1})

          roll-1-moves        [(bg/->Move 24 23)
                               (bg/->Move 8 7)
                               (bg/->Move 6 5)]

          roll-2-moves        [(bg/->Move 24 22)
                               (bg/->Move 13 11)
                               (bg/->Move 8 6)
                               (bg/->Move 6 4)]

          valid-combos        (for [m1 roll-1-moves
                                    m2 roll-2-moves
                                    :when (not= (:from m1) (:from m2))]
                                #{[m1 m2] [m2 m1]})

          same-man-moves      #{[(bg/->Move 24 23) (bg/->Move 23 21)]
                                [(bg/->Move 24 22) (bg/->Move 22 21)]
                                [(bg/->Move 13 11) (bg/->Move 11 10)]
                                [(bg/->Move 8  7)  (bg/->Move 7  5)]
                                [(bg/->Move 8  6)  (bg/->Move 6  5)]
                                [(bg/->Move 6  5)  (bg/->Move 5  3)]
                                [(bg/->Move 6  4)  (bg/->Move 4  3)]}

          expected            (reduce set/union #{} (conj valid-combos same-man-moves))]

      (t/is (m/validate s/Board board))
      (t/is (m/validate s/LegalMoves expected))
      (t/is (m/validate s/LegalMoves legal-moves))

      (t/is (= expected legal-moves))))

  (t/testing "player on the bar"
    (let [{:keys [p1 p2]}     (bg/player-generator)
          board               (->
                               bg/initial-setup
                               (assoc :point->men (u/deep-merge  bg/empty-point->men
                                                                 {1    [(p1)]
                                                                  :bar {:p2 [(p2)]}})))
          legal-moves         (bg/get-legal-moves {:board board :die-rolls [1 2] :player :p2})
          expected            #{[(bg/->Move [:bar :p2] 2)]
                                [(bg/->Move [:bar :p2] 1)]}]

      ;; (println (-> s/Board
      ;;              (m/explain board)
      ;;              (me/humanize)))

      (t/is (m/validate s/Board board))
      (t/is (m/validate s/LegalMoves expected))
      (t/is (m/validate s/LegalMoves legal-moves))

      (t/is (= expected legal-moves))))

  (t/testing "taking"
    (let [{:keys [p1 p2]}     (bg/player-generator)
          board               (-> bg/initial-setup
                                  (assoc :point->men (u/deep-merge  bg/empty-point->men
                                                                    {1 [(p2)]
                                                                     2 [(p1)]})))

          legal-moves         (bg/get-legal-moves {:board board :die-rolls [1 2] :player :p2})
          expected           #{[(bg/->Move 1 2)] [(bg/->Move 1 3)]}]

      (t/is (m/validate s/Board board))
      (t/is (m/validate s/LegalMoves expected))
      (t/is (m/validate s/LegalMoves legal-moves))

      (t/is (= expected legal-moves))))

  (t/testing "bearing off"
    (let [{:keys [p1 p2]}     (bg/player-generator)
          board               (-> bg/initial-setup
                                  (assoc :point->men (u/deep-merge bg/empty-point->men
                                                                   {1 [(p1)]})))

          legal-moves         (bg/get-legal-moves {:board board :die-rolls [3 2] :player :p1})
          expected            #{[(bg/->Move 1 :borne-off)]}]

      (t/is (m/validate s/Board board))
      (t/is (m/validate s/LegalMoves expected))
      (t/is (m/validate s/LegalMoves legal-moves))

      (t/is (= expected legal-moves))))

  (t/testing "rolling a double"
    (let [{:keys [p1 p2]}   (bg/player-generator)
          board             (empty-board-with {12 [(p2)]
                                               6  [(p1)]})
          die-rolls         [1 1]
          legal-moves-p1    (bg/get-legal-moves {:board board :die-rolls die-rolls :player :p1})
          expected-p1       #{[(bg/->Move 6 5)
                               (bg/->Move 5 4)
                               (bg/->Move 4 3)
                               (bg/->Move 3 2)]}

          legal-moves-p2    (bg/get-legal-moves {:board board :die-rolls die-rolls :player :p2})
          expected-p2       #{[(bg/->Move 12 13)
                               (bg/->Move 13 14)
                               (bg/->Move 14 15)
                               (bg/->Move 15 16)]}]
      (t/is (= expected-p1 legal-moves-p1))
      (t/is (= expected-p2 legal-moves-p2))))

  (t/testing "moving same man multiple times"
    (let [{:keys [p1 p2]}   (bg/player-generator)
          board             (empty-board-with {13 [(p2)]
                                               18 [(p2)]
                                               6  [(p1)]
                                               12 [(p1)]})
          die-rolls         [1 2]
          legal-moves-p1    (bg/get-legal-moves {:board board :die-rolls die-rolls :player :p1})
          expected-p1       #{[(bg/->Move 6 5) (bg/->Move 5 3)]
                              [(bg/->Move 6 4) (bg/->Move 4 3)]
                              [(bg/->Move 12 11) (bg/->Move 11 9)]
                              [(bg/->Move 12 10) (bg/->Move 10 9)]

                              [(bg/->Move 12 11) (bg/->Move 6 4)]
                              [(bg/->Move 6 4)   (bg/->Move 12 11)]

                              [(bg/->Move 12 10) (bg/->Move 6 5)]
                              [(bg/->Move 6 5)   (bg/->Move 12 10)]}

          legal-moves-p2    (bg/get-legal-moves {:board board :die-rolls die-rolls :player :p2})
          expected-p2       #{[(bg/->Move 13 14) (bg/->Move 14 16)]
                              [(bg/->Move 13 15) (bg/->Move 15 16)]
                              [(bg/->Move 18 19) (bg/->Move 19 21)]
                              [(bg/->Move 18 20) (bg/->Move 20 21)]

                              [(bg/->Move 13 14) (bg/->Move 18 20)]
                              [(bg/->Move 18 20) (bg/->Move 13 14)]

                              [(bg/->Move 13 15) (bg/->Move 18 19)]
                              [(bg/->Move 18 19) (bg/->Move 13 15) ]}]
      (t/is (= expected-p1 legal-moves-p1))
      (t/is (= expected-p2 legal-moves-p2))))

  (t/testing "blocked from moving off the bar - p2"
    (let [{:keys [p1 p2]}   (bg/player-generator)
          board             (empty-board-with {:bar {:p2 [(p2)]}
                                               6 [(p1) (p1)]
                                               5 [(p1) (p1)]})
          die-rolls         [6 5]
          legal-moves       (bg/get-legal-moves {:board board :die-rolls die-rolls :player :p2})
          expected          #{}]

      (t/is (= expected legal-moves))
      (t/is (= expected legal-moves))))

  (t/testing "blocked from moving off the bar - p1"
    (let [{:keys [p1 p2]}   (bg/player-generator)
          board             (empty-board-with {:bar {:p1 [(p1)]}
                                               19 [(p2) (p2)]
                                               20 [(p2) (p2)]})
          die-rolls         [6 5]
          legal-moves       (bg/get-legal-moves {:board board :die-rolls die-rolls :player :p1})
          expected          #{}]

      (t/is (= expected legal-moves))
      (t/is (= expected legal-moves))))

  (t/testing "can play one or other die, but not both - p1"
    (let [{:keys [p1 p2]}   (bg/player-generator)
          board             (empty-board-with {3 [(p2) (p2)]
                                               6 [(p1)]})
          die-rolls         [1 2]
          legal-moves       (bg/get-legal-moves {:board board :die-rolls die-rolls :player :p1})
          expected          #{[(bg/->Move 6 4)]}]

      (t/is (= expected legal-moves))
      (t/is (= expected legal-moves))))

  (t/testing "can play one or other die, but not both - p2"
    (let [{:keys [p1 p2]}   (bg/player-generator)
          board             (empty-board-with {19 [(p2)]
                                               22 [(p1) (p1)]})
          die-rolls         [1 2]
          legal-moves       (bg/get-legal-moves {:board board :die-rolls die-rolls :player :p2})
          expected          #{[(bg/->Move 19 21)]}]

      (t/is (= expected legal-moves))
      (t/is (= expected legal-moves)))))

(t/deftest test-get-legal-moves-1
  (t/testing "basic moves"
    (let [{:keys [p1 p2]}  (bg/player-generator)
          board            (empty-board-with {12 [(p1) (p1)]
                                              15 [(p1)]
                                              5  [(p2) (p2)]
                                              14 [(p2)]})
          die-roll         2
          moves-p1         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p1})
          expected-p1      #{(bg/->Move 12 10) (bg/->Move 15 13)}
          moves-p2         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p2})
          expected-p2      #{(bg/->Move 5 7) (bg/->Move 14 16)}]
      (t/is (= expected-p1 moves-p1))
      (t/is (= expected-p2 moves-p2))))

  (t/testing "taking"
    (let [{:keys [p1 p2]}  (bg/player-generator)
          board            (empty-board-with {12 [(p2)]
                                              15 [(p1) (p1)]
                                              5  [(p2)]
                                              8  [(p1)]})
          die-roll         3
          moves-p1         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p1})
          expected-p1      #{(bg/->Move 15 12)}
          moves-p2         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p2})
          expected-p2      #{(bg/->Move 5 8)}]
      (t/is (= expected-p1 moves-p1))
      (t/is (= expected-p2 moves-p2))))

  (t/testing "blocked"
    (let [{:keys [p1 p2]}  (bg/player-generator)
          board            (empty-board-with {12 [(p2) (p2)]
                                              15 [(p1) (p1)]
                                              5  [(p2)]
                                              8  [(p1) (p1)]})
          die-roll         3
          moves-p1         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p1})
          expected-p1      #{}
          moves-p2         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p2})
          expected-p2      #{}]
      (t/is (= expected-p1 moves-p1))
      (t/is (= expected-p2 moves-p2))))

  (t/testing "re-entry"
    (let [{:keys [p1 p2]}  (bg/player-generator)
          board            (empty-board-with {:bar {:p1 [(p1)] :p2 [(p2)]}
                                              12 [(p1)]
                                              13 [(p2)]})
          die-roll         6
          moves-p1         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p1})
          expected-p1      #{(bg/->Move [:bar :p1] 6)}
          moves-p2         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p2})
          expected-p2      #{(bg/->Move [:bar :p2] 19)}]
      (t/is (= expected-p1 moves-p1))
      (t/is (= expected-p2 moves-p2))))

  (t/testing "blocked re-entry"
    (let [{:keys [p1 p2]}  (bg/player-generator)
          board            (empty-board-with {:bar {:p1 [(p1)] :p2 [(p2)]}
                                              19 [(p1) (p1)]
                                              6  [(p2) (p2)]})
          die-roll         6
          moves-p1         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p1})
          expected-p1      #{}
          moves-p2         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p2})
          expected-p2      #{}]
      (t/is (= expected-p1 moves-p1))
      (t/is (= expected-p2 moves-p2))))

  (t/testing "taking re-entry"
    (let [{:keys [p1 p2]}  (bg/player-generator)
          board            (empty-board-with {:bar {:p1 [(p1)] :p2 [(p2)]}
                                              19 [(p1)]
                                              6  [(p2)]})
          die-roll         6
          moves-p1         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p1})
          expected-p1      #{(bg/->Move [:bar :p1] 6)}
          moves-p2         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p2})
          expected-p2      #{(bg/->Move [:bar :p2] 19)}]
      (t/is (= expected-p1 moves-p1))
      (t/is (= expected-p2 moves-p2))))

  (t/testing "bearing off not allowed because player on bar"
    (let [{:keys [p1 p2]}  (bg/player-generator)
          board            (empty-board-with {:bar {:p1 [(p1)] :p2 [(p2)]}
                                              24 [(p2) (p2)]
                                              1  [(p1) (p1)]})
          die-roll         1
          moves-p1         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p1})
          expected-p1      #{}
          moves-p2         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p2})
          expected-p2      #{}]
      (t/is (= expected-p1 moves-p1))
      (t/is (= expected-p2 moves-p2))))

  (t/testing "bearing off"
    (let [{:keys [p1 p2]}  (bg/player-generator)
          board            (empty-board-with {24 [(p2) (p2)]
                                              1  [(p1) (p1)]})
          die-roll         1
          moves-p1         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p1})
          expected-p1      #{(bg/->Move 1 :borne-off)}
          moves-p2         (bg/get-legal-moves-1 {:board board :die-roll die-roll :player :p2})
          expected-p2      #{(bg/->Move 24 :borne-off)}]
      (t/is (= expected-p1 moves-p1))
      (t/is (= expected-p2 moves-p2)))))

;; todo: need a much more expressive language to describe cases and expectations
(t/deftest test-apply-move
  (t/testing "basic moves"
    (let [board      bg/initial-setup
          move       (bg/->Move 24 23)
          next-board (bg/apply-move board move)]

      (t/is (m/validate s/Board next-board))

      (t/is (= [(first (get-in board [:point->men 24]))]
               (get-in next-board [:point->men 24])))

      (t/is (= [(second (get-in board [:point->men 24]))]
               (get-in next-board [:point->men 23])))))

  (t/testing "taking"
    (let [id-atom   (atom 0)
          p1        (fn [] (bg/make-man :p1 id-atom))
          p2        (fn [] (bg/make-man :p2 id-atom))

          base-board bg/initial-setup
          point->men (merge bg/empty-point->men {24 [(p1) (p1)]
                                                 20 [(p2)]})
          board      (assoc base-board :point->men point->men)

          move       (bg/->Move 24 20)

          next-board (bg/apply-move board move)]

      (t/is (m/validate s/Board next-board))

      (t/is (= (u/deep-merge bg/empty-point->men
                             {24 (pop (get point->men 24))
                              20 [(peek (get point->men 24))]
                              :bar {:p2 [(peek (get point->men 20))]}})
               (get-in next-board [:point->men])))))

  (t/testing "bearing off"
    (let [id-atom   (atom 0)
          p1        (fn [] (bg/make-man :p1 id-atom))

          base-board bg/initial-setup
          point->men (merge bg/empty-point->men {2 [(p1) (p1)]
                                                 3 [(p1)]})
          board      (assoc base-board :point->men point->men)

          move       (bg/->Move 2 :borne-off)

          next-board (bg/apply-move board move)]

      (t/is (m/validate s/Board next-board))

      (t/is (= (merge bg/empty-point->men {2 (pop (get point->men 2))
                                           3 (get point->men 3)
                                           :borne-off [(peek (get point->men 2))]})
               (get-in next-board [:point->men])))

      (t/is (= {:p1 [] :p2 []}
               (get-in next-board [:point->men :bar])))))

  (t/testing "re-enter from bar"
    (let [id-atom   (atom 0)
          p1        (fn [] (bg/make-man :p1 id-atom))
          p2        (fn [] (bg/make-man :p2 id-atom))
          p1-bar    (p1)
          p2-bar    (p2)

          base-board bg/initial-setup
          point->men (u/deep-merge bg/empty-point->men
                                   {2 [(p1) (p1)]
                                    :bar {:p1 [p1-bar] :p2 [p2-bar]}})
          board      (-> base-board
                         (assoc :point->men point->men)
                         (assoc :bar [p2-bar p1-bar]))

          move       (bg/->Move [:bar :p2] 3)

          next-board (bg/apply-move board move)]

      (t/is (m/validate s/Board next-board))

      (t/is (= (u/deep-merge point->men
                             {3 [p2-bar]
                              :bar {:p1 [p1-bar] :p2 []}})
               (get-in next-board [:point->men]))))))

(comment
  (t/deftest grok-order
    (t/testing "assertion order"
      (t/is (= :expected :actual))))

  (->> (for [m1 [:a1 :b1 :c1]
             m2 [:a2 :b2 :c2]]
         #{[m1 m2] [m2 m1]})
       (reduce clojure.set/union #{}))

  (as-> #{[1 2] [3 4]} $
    (conj [#{[:a :b] [:b :a]} #{[:b :a] [:c :d]}] $)
    (reduce set/union #{} $)
    (into [] $)
    (rand-nth $))

  (pop [1 2])
  ;;
  )
