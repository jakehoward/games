(ns org.jakehoward.backgammon.utils-test
  (:require [clojure.test :as t]
            [org.jakehoward.backgammon.utils :as u]))

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

(take 20 (repeatedly u/roll-die))

;; C-c C-t n => namespace tests
;; t/is = :expected => :actual
(t/deftest die
  (t/testing "uniformly distributed numbers in range [1, 6]"
    (let [num-rolls 500
          rolls     (repeatedly num-rolls u/roll-die)]
      (t/is (every? #{1 2 3 4 5 6} rolls))
      (t/is (uniformly-distributed? rolls 6)))))

(t/deftest deep-merge
  (t/testing "emptyness"
    (t/is (= [] (u/deep-merge [] [])))
    (t/is (= {} (u/deep-merge {} {})))
    (t/is (= [] (u/deep-merge '() '()))))

  (t/testing "scalars"
    (t/is (= 1 (u/deep-merge {} 1)))
    (t/is (= "1" (u/deep-merge {} "1")))
    (t/is (= :a (u/deep-merge {} :a)))
    (t/is (= 1 (u/deep-merge [] 1)))
    (t/is (= "1" (u/deep-merge [] "1")))
    (t/is (= :a (u/deep-merge [] :a)))
    (t/is (= 1 (u/deep-merge '() 1)))
    (t/is (= "1" (u/deep-merge '() "1")))
    (t/is (= :a (u/deep-merge '() :a))))

  (t/testing "simple cases"
    (t/is (= {:a 1 :b 2}
             (u/deep-merge {:a 1}
                           {:b 2})))

    (t/is (= {:a 1 :b {:c 3 :d 4}}
             (u/deep-merge {:a 1 :b {:c 3}}
                           {:b {:d 4}})))

    (t/is (= {:a 1 :b 2}
             (u/deep-merge {:a 1 :b {:c 3}}
                           {:b 2})))

    (t/is (= {:a {:b 2}}
             (u/deep-merge {:a 1}
                           {:a {:b 2}})))

    (t/is (= {:a 1 :b {:c 3} :d {:e {:f 3 :g 4}}}
             (u/deep-merge {:a 1 :b {:c 2} :d {:e {:f 3}}}
                           {:a 1 :b {:c 3} :d {:e {:g 4}}})))

    (t/is (= {:a 1 :b [:B :B]}
             (u/deep-merge {:a 1 :b {:c 2}}
                           {:b [:B :B]}))))

  (t/testing "array lhs"
    (t/is (= {:foo :bar}
             (u/deep-merge [:a :b]
                           {:foo :bar})))))
