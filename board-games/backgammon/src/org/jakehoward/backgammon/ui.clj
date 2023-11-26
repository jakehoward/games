{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns org.jakehoward.backgammon.ui
  (:require [nextjournal.clerk :as clerk]
            [org.jakehoward.backgammon.core :as bg]
            [org.jakehoward.backgammon.constants :as c]
            [clojure.string :as str]))

(defonce x (clerk/serve! {:browse true}))

(defn half [x] (/ x 2))

(def board-width 700)
(def middle (half board-width))
(def board-height 550)
(def bar-width 30)

(def point-width (/ (- middle (half bar-width)) 6))
(defn dfy [path] (str/join " " path))
(def c1 "#a39d9d")
(def c2 "#524545")

(def man-radius 25)
(def p1-colour "#dede9e")
(def p2-colour "#db93ac")

(def background [:rect {:width board-width :height board-height :fill "#2e2b2b"}])

(def bar [:rect
          {:width bar-width
           :height board-height
           :x (- middle (half bar-width))
           :y 0
           :fill "#423e3e"}])

(def point [:path {:d (dfy
                       ["M" "0" "0"
                        "L" (int point-width) "0"
                        "L" (int (half point-width)) (- (half board-height) 20)])
                   :fill "blue"}])

(defn transform [e t]
  (let [[tag attrs & others] e]
    (into [tag
           (update attrs :transform (fn [e] (str/trim (str/join " " [(or e "") t]))))]
          others)))

(defn translate [e x y]
  (transform e (format "translate(%s,%s)" (int x) (int y))))

(defn rotate [e angle]
  (transform e (format "rotate(%s)" angle)))

(defn fill [e c]
  [(first e) (assoc (second e) :fill c)])

(def point-group-width (* 6 point-width))

(def point-group
  [:g
   {}
   (fill point c1)
   (fill (translate point (int point-width) 0) c2)
   (fill (translate point (int (* 2 point-width)) 0) c1)
   (fill (translate point (int (* 3 point-width)) 0) c2)
   (fill (translate point (int (* 4 point-width)) 0) c1)
   (fill (translate point (int (* 5 point-width)) 0) c1)])

(defn man [player]
  (let [colour (if (= :p1 player) p1-colour p2-colour)]
    [:circle
     {:cx 0 :cy 0 :r man-radius :fill colour :filter "url(#shadow)"}]))

(defn move-man-to-point [man point depth num-men]
  (let [overlap (if (> num-men 6) (int (- (* 2 man-radius)
                                          (/ (* 2 man-radius 5) (dec num-men))))
                    0)]
    (translate man
               (if (>= point 13)
                 (+ (int (+ man-radius (* (- point 13) point-width)))
                    (if (> point 18) bar-width 0))
                 (+ (int (+ man-radius (* (- 12 point) point-width)))
                    (if (< point 7) bar-width 0)))
               (if (>= point 13)
                 (int (- (+ man-radius (* depth (* 2 man-radius))) (* depth overlap)))
                 (int (+ (* depth overlap) (- board-height (+ man-radius (* depth (* 2 man-radius))))))))))

(defn move-man-to-bar [man depth player num-men-on-bar]
  (translate man
             (int middle)
             (if (= :p1 player)
               (- board-height (* 2 man-radius) (* depth man-radius))
               (+ (* 2 man-radius) (* depth man-radius)))))

(defn bar-men-svg [men player]
  (->> men
       (map (fn [idx m] (-> (man (:player m))
                            (move-man-to-bar idx player (count m))))
            (range))))

(defn board->bar-men-svg [board]
  (let [p1-bar (get-in board [:point->men :bar :p1])
        p2-bar (get-in board [:point->men :bar :p2])]
    [:g {}
     (bar-men-svg p1-bar :p1)
     (bar-men-svg p2-bar :p2)]))

(defn board->point-men-svg [board]
  (->> (:point->men board)
       (filter (fn [[point men]] (int? point)))
       (mapcat (fn [[point men]]
                 (->> men
                      (map (fn [idx m] (-> (man (:player m))
                                           (move-man-to-point point idx (count men))))
                           (range 0 (count men))))))
       (into [:g {}])))

(defn board->svg [board]
  [:svg {:width board-width :height board-height}
   [:defs
    [:filter {:id "shadow"}
     [:feDropShadow {:dx "0.9" :dy "0.9" :stdDeviation "0.2"}]]]
   background
   bar
   point-group
   (translate point-group (int (+ bar-width point-group-width)) 0)
   (-> point-group
       (translate (int point-group-width) (int board-height))
       (rotate 180))
   (-> point-group
       (translate (int (+ bar-width (* 2 point-group-width))) (int board-height))
       (rotate 180))
   (board->point-men-svg board)
   (board->bar-men-svg board)])

(defn board->header [board]
  (let [borne-off   (get-in board [:point->men :borne-off])
        grouped     (group-by :player borne-off)
        p1-count    (-> grouped :p1 count)
        p2-count    (-> grouped :p2 count)
        idx->colour (fn [idx player] (if (> (if (= :p1 player) p1-count p2-count) idx)
                                       (if (= :p1 player) p1-colour p2-colour) "dark-gray"))]
    [:div {:style {:display :flex :flex-direction :row :justify-content :space-between}}

     [:div {:style {:display :flex :flex-direction :column :gap 10}}
      "Borne off"
      (into [:div {:style {:display :flex :flex-direction :row :gap 3}}
             [:div {:style {:margin-right 10}} "Player 1"]]

            (map (fn [idx] [:div {:style {:width 10
                                          :height 20
                                          :border-color "lightgray"
                                          :border-width 1
                                          :background-color (idx->colour idx :p1)
                                          :color            (idx->colour idx :p1)}}
                            " "])
                 (range c/men-per-player)))
      (into [:div {:style {:display :flex :flex-direction :row :gap 3}}
             [:div {:style {:margin-right 10}} "Player 2"]]

            (map (fn [idx] [:div {:style {:width 10
                                          :height 20
                                          :border-color "lightgray"
                                          :border-width 1
                                          :background-color (idx->colour idx :p2)
                                          :color            (idx->colour idx :p2)}}
                            " "])
                 (range c/men-per-player)))]

     [:div {:style {:display :flex :flex-direction :column :gap 10 :align-self :center}}
      [:div {:style {:display :flex :flex-direction :row :gap 10 :align-items :center}}
       [:div "Player 1"]
       [:div {:style {:background-color p1-colour :color p1-colour :height 20 :width 20}} " "]]
      [:div {:style {:display :flex :flex-direction :row :gap 10 :align-items :center}}
       [:div "Player 2"]
       [:div {:style {:background-color p2-colour :color p2-colour :height 20 :width 20}} " "]]]]))

(defn board->html [board]
  [:div {:style {:display :flex :flex-direction :column :gap 20}}
   (board->header board)
   (board->svg board)])

{:nextjournal.clerk/visibility {:code :show :result :show}}
(clerk/html (board->html bg/initial-setup))

(let [id-atom (atom 0)
      p1      (fn [] (bg/make-man :p1 id-atom))
      p2      (fn [] (bg/make-man :p2 id-atom))
      board   (merge bg/initial-setup {:point->men {:borne-off [(p1) (p2) (p1) (p1)]
                                                    :bar       {:p1 [(p1) (p1)]
                                                                ;; :p1 (into [] (repeatedly 7 p1))
                                                                ;; :p2 (into [] (repeatedly 7 p2))
                                                                :p2 [(p2) (p2) (p2)]}
                                                    1          [(p1)]
                                                    22         [(p2)]}})]
  (clerk/html (board->html board)))

{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(comment
  (clerk/html (board->html
               (assoc bg/initial-setup :point->men
                      {18 (into [] (repeat 10 {:player :p1 :id :we}))}))))
