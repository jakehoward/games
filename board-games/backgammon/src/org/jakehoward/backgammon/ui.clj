(ns org.jakehoward.backgammon.ui
  (:require
   [io.github.humbleui.canvas :as canvas]
   [io.github.humbleui.core :as core]
   [io.github.humbleui.paint :as paint]
   [io.github.humbleui.ui :as ui]
   [io.github.humbleui.window :as window])
  (:import
   [io.github.humbleui.skija Canvas Color PaintMode Path]))


(defonce *window
  (atom nil))


(defn fill-cell [canvas x y paint opacity]
  (.setAlpha paint opacity)
  (canvas/draw-rect canvas (core/rect-xywh x y 1 1) paint))

(def paint-knot
  (paint/stroke 0xFF000000 0.1))

(defn render-sth [canvas x y]
  (fill-cell canvas x y (paint/fill 0) 255))


(defn paint [ctx canvas size]
  (let [field       (min (:width size) (:height size))
        dim-x        50
        dim-y        40
        ;; translate-x  25
        ;; translate-y  -20
        scale-x      (/ field dim-x)
        scale-y      (/ field dim-y)]

    ;; center canvas
    (canvas/translate canvas
                      (-> (:width size) (- field) (/ 2))
                      (-> (:height size) (- field) (/ 2)))

    ;; scale to fit full width/height but keep square aspect ratio
    (canvas/scale canvas scale-x scale-y)

    ; erase background
    (with-open [bg (paint/fill 0xFFFFFFFF)]
      (canvas/draw-rect canvas (core/rect-xywh 0 0 dim-x dim-y) bg))

    (render-sth canvas 0 0)

    ;; schedule redraw on next vsync
    (window/request-frame (:window ctx))))

(def app
  (-> (ui/canvas {:on-paint paint})
      ui/center
      ui/default-theme))

(defn start! []
  (->> (ui/window {:title "Backgammon"} #'app)
       (reset! *window)
       (ui/start-app!)))

(defn -main [& args]
  (start!))

(comment
  (start!)
  ;
  )
