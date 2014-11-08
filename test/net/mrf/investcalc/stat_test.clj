(ns
  ^{:author mrf
    :doc "unit tests for net.mrf.investcalc.stat"}
  net.mrf.investcalc.stat-test
  (:use clojure.test)
  (:require [net.mrf.investcalc.stat :as stat]
            [clojure.math.numeric-tower :as math]))

(deftest test-mean-n
  (is (= [1.1M 3] (stat/mean-n [1.1M 1.1M 1.1M])))
  (is (= [0 0] (stat/mean-n [])) "empty series")
  (is (= [1.1M 1] (stat/mean-n [1.1M])) "series with single value")
  (is (= [-0.2833333333M 6]
        (with-precision 10 :rounding HALF_EVEN (stat/mean-n [-1.1M 0.1M 0.1M, 0.1M, -1.1M, 0.2M])))
        "result with non-terminating decimal representation"))

(deftest test-sum-n
  (is (= [3.3M 3] (stat/sum-n [1.1M 1.1M 1.1M])) "series of the same value")
  (is (= [0 0] (stat/sum-n [])) "empty series")
  (is (= [1010.192347M 1] (stat/sum-n [1010.192347M])) "series with single value")
  (is (= [1.09000001M 3] (stat/sum-n [1.1M -0.01M 0.00000001M]))))

(deftest test-variance-p
  (is (= 0.00M (with-precision 2 :rounding HALF_EVEN (stat/variance-p [0.0M]))))
  (is (= 0.00M (stat/variance-p [0.1M 0.1M 0.1M 0.1M 0.1M])) "series of the same value")
  (is (= 0.06401875M (with-precision 8 :rounding HALF_EVEN
         (stat/variance-p [0.1M, 0.2M, 0.01M, 0.666666667M])))))

(deftest test-variance-s
  (is (= 0.00M (stat/variance-s [0.1M 0.1M 0.1M 0.1M 0.1M])) "series of the same value")
  (is (= 0.085358333M (with-precision 8 :rounding HALF_EVEN
         (stat/variance-s [0.1M, 0.2M, 0.01M, 0.666666667M])))))

(deftest test-stddev-p
  (is (= 0 (stat/stddev-p [0.0M])))
  (is (= 0 (stat/stddev-p [0.1M 0.1M 0.1M 0.1M 0.1M])) "series of the same value")
  (is (= 0.25301927M (with-precision 16 :rounding HALF_EVEN
          (stat/scaled-decimal 8 (stat/stddev-p [0.1M 0.2M 0.01M 0.666666667M]))))))

(deftest test-stddev-s
  (is (= 0 (stat/stddev-s [0.1M 0.1M 0.1M 0.1M 0.1M])) "series of the same value")
  (is (= 0.29216149M (with-precision 16 :rounding HALF_EVEN
          (stat/scaled-decimal 8 (stat/stddev-s [0.1M 0.2M 0.01M 0.666666667M]))))))