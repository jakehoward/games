(ns org.jakehoward.backgammon.utils)

(def num-die-sides 6)
(defn roll-die []
  (inc (rand-int num-die-sides)))

(defn remove-one [pred coll]
  (let [before (take-while #(not (pred %)) coll)
        after  (drop (inc (count before)) coll)]
    ;; do I need to watch out? https://stuartsierra.com/2015/04/26/clojure-donts-concat
    (concat before after)))

(defn deep-merge [l r]
  (cond (and (map? l) (map? r))
        (reduce (fn [m [k v]]
                  (cond (and (map? (get m k)) (map? v))
                        (assoc m k (deep-merge (get m k) v))

                        :else
                        (assoc m k v)))
                l r)

        :else r))

(comment

  (defrecord MyRec [a])
  (assoc (->MyRec :a) :b :A)
  (remove-one even? [1 2 3 4 5])

  (associative? []) ;; => true
  (associative? '()) ;; => false

  (map? []) ;; => false
  (map? (->MyRec :a)) ;; => true
  (map? {}) ;; => true
  (map?)

  (sequential? {}) ;; => false
  (seqable? {}) ;; => true
  (sequential? (->MyRec :a)) ;; => false
  (sequential? []) ;; => true
  (sequential? '()) ;; => true
  (type (empty '(:a :b))))
