(ns org.jakehoward.backgammon.utils-test
  (:require [clojure.test :as t]
            [org.jakehoward.backgammon.utils :as utils]))

(defn uniformly-distributed? [ns num-categories]
  ;; In lieu of a Chi-Squared test
  (let [freqs         (-> (frequencies ns) vals)
        num-samples   (count ns)
        ideal-prop    (/ 1 num-categories)
        lower-bound   (- ideal-prop 0.05)
        upper-bound   (+ ideal-prop 0.05)]

    (if (not= (count freqs) num-categories)
      false
      (every? #(< lower-bound (/ % num-samples) upper-bound) freqs))))

(take 20 (repeatedly utils/roll-die))

;; C-c C-t n => namespace tests
(t/deftest die
  (t/testing "uniformly distributed numbers in range [1, 6]"
    (let [num-rolls 500
          rolls     (repeatedly num-rolls utils/roll-die)]
      (t/is (every? #{1 2 3 4 5 6} rolls))
      (t/is (uniformly-distributed? rolls 6)))))
