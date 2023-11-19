(ns org.jakehoward.backgammon.core-test
  (:require [org.jakehoward.backgammon.core :as bg]
            [clojure.set :as set]
            [clojure.test :as t]))

;; property based ideas
;; - if not bearning off, number of men on board invariant
;; - num men total is invariant
;; - if bearning off, number of men removed <= 2 unless double <= 4
;; - if not bearning off, all legal moves from - to = die roll, die roll only used once

(t/deftest test-get-legal-moves
  (t/testing "initial board"
    (let [board               bg/initial-setup

          legal-moves         (bg/get-legal-moves bg/p1? {:board board :die-rolls [1 2]})

          roll-1-moves        [(bg/->Move 24 23)
                               (bg/->Move 8 7)
                               (bg/->Move 6 5)]

          roll-2-moves        [(bg/->Move 24 22)
                               (bg/->Move 13 11)
                               (bg/->Move 8 6)
                               (bg/->Move 6 4)]

          valid-combos         (for [m1 roll-1-moves
                                     m2 roll-2-moves
                                     :when (not= (:from-point m1) (:from-point m2))
                                     ]
                                 #{[m1 m2] [m2 m1]})

          same-man-moves      #{[(bg/->Move 24 23) (bg/->Move 23 21)]
                                [(bg/->Move 24 22) (bg/->Move 22 21)]
                                [(bg/->Move 13 11) (bg/->Move 11 10)]
                                [(bg/->Move 8  7)  (bg/->Move 7  5)]
                                [(bg/->Move 8  6)  (bg/->Move 6  5)]
                                [(bg/->Move 6  5)  (bg/->Move 5  3)]
                                [(bg/->Move 6  4)  (bg/->Move 4  3)]}

          expected           (reduce set/union #{} (conj valid-combos same-man-moves))]

      (t/is (= expected legal-moves)))))

(comment
  (->> (for [m1 [:a1 :b1 :c1]
             m2 [:a2 :b2 :c2]]
         #{[m1 m2] [m2 m1]})
       (reduce clojure.set/union #{}))

  (as-> #{[1 2] [3 4]} $
    (conj [#{[:a :b] [:b :a]} #{[:b :a] [:c :d]}] $)
    (reduce set/union #{} $)
    (into [] $)
    (rand-nth $))
  ;;
  )
