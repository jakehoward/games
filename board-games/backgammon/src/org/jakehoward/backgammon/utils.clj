(ns org.jakehoward.backgammon.utils)

(defn roll-die []
  (inc (rand-int 6)))

(defn remove-one [pred coll]
  (let [before (take-while #(not (pred %)) coll)
        after  (drop (inc (count before)) coll)]
    ;; do I need to watch out? https://stuartsierra.com/2015/04/26/clojure-donts-concat
    (concat before after)))

(comment
  (remove-one even? [1 2 3 4 5])
  )
