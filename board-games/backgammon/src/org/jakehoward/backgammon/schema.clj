(ns org.jakehoward.backgammon.schema
  (:require [malli.core :as m]
            [malli.util :as mu]
            [org.jakehoward.backgammon.constants :as c]))

(defn minmax [s min max]
  (-> s
      (mu/update-properties assoc :min min)
      (mu/update-properties assoc :max max)))

(def Player
  (m/schema [:enum :p1 :p2]))

(def Man
  (m/schema
   [:map
    [:player Player]
    [:id     [:int {:min 1}]]]))

(def Men
  (m/schema [:vector Man])) ;; todo: unique ids

(def Point->Men
  (m/schema
   [:map
    [:borne-off (minmax Men 0 (* 2 c/men-per-player))]
    [:bar [:map
           [:p1 (minmax Men 0 c/men-per-player)]
           [:p2 (minmax Men 0 c/men-per-player)]]]
    [::m/default [:map-of
                  [:and :int [:> 0] [:< 25]] (minmax Men 0 c/men-per-player)]]]))

(def Board
  (m/schema
   [:map
    [:point->men Point->Men]]))

(def Move
  (m/schema
   [:or
    ;; Can only bear off from home
    [:map
     [:from [:or [:int {:min 1 :max 6}] [:int {:min 19 :max 24}]]]
     [:to [:= :borne-off]]]

    ;; Can only re-enter to other player's home
    [:map
     [:from [:= [:bar :p1]]]
     [:to [:int {:min 19 :max 24}]]]

    [:map
     [:from [:= [:bar :p2]]]
     [:to [:int {:min 1 :max 6}]]]

    ;; Other points
    [:map
     [:from [:int {:min 1 :max 24}]]
     [:to [:int {:min 1 :max 24}]]]]))

(def LegalMoves
  (m/schema
   [:set
    [:vector {:min 0 :max 4} Move]]))

(def DieRolls
  (m/schema
   [:vector {:min 2 :max 2} [:int {:min 1 :max 6}]]))

(comment

  (m/schema? [:string {:min 1}]) ;; => false
  (m/schema? (m/schema [:string {:min 1}])) ;; => true
  (meta (m/schema [:string {:min 1}])) ;; => {:line 668, :column 15, :type :malli.core/schema}
  )

