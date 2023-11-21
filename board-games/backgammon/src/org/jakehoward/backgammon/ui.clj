(ns org.jakehoward.backgammon.ui
  (:require [nextjournal.clerk :as clerk]
            [org.jakehoward.backgammon.core :as bg]
            [clojure.string :as str]))

(defonce x (clerk/serve! {:browse true}))

(defn half [x] (/ x 2))

(def board-width 700)
(def middle (half board-width))
(def board-height 400)
(def bar-width 30)
(def point-width (/ (- middle (half bar-width)) 6))
(defn dfy [path] (str/join " " path))
(def c1 "#a39d9d")
(def c2 "#524545")


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
                        "L" (int (half point-width)) (- (half board-height) 10)])
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

(defn board->svg [board]
  [:svg {:width board-width :height board-height}
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
   ;; [:circle {:cx 0 :cy 0 :r 25 :fill "blue"}]
   ;; [:circle {:cx 100 :cy 75 :r 25 :fill "red"}]
   ])


(clerk/html (board->svg bg/initial-setup))

(comment
  )
