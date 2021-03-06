(ns
  ^{:author mrf
    :doc "unit tests for net.mrf.investcalc.stat"}
  net.mrf.investcalc.stat-test
  (:use clojure.test)
  (:require [net.mrf.investcalc.stat :as stat]
            [clojure.math.numeric-tower :as math]))

(deftest test-mean-n
  (is (= [1.1M 3] (stat/mean-n [1.1M 1.1M 1.1M])) "series of the same value")
  (is (= [0 0] (stat/mean-n [])) "empty series")
  (is (= [1.1M 1] (stat/mean-n [1.1M])) "series with single value")
  (is (= [-0.2833333333M 6]
        (with-precision 10 :rounding HALF_EVEN (stat/mean-n [-1.1M 0.1M 0.1M, 0.1M, -1.1M, 0.2M])))
        "result with non-terminating decimal representation")
  (is (= [2 3] (stat/mean-n (filter even? (range 5)))) "lazy sequence"))

(deftest test-sum-n
  (is (= [3.3M 3] (stat/sum-n [1.1M 1.1M 1.1M])) "series of the same value")
  (is (= [0 0] (stat/sum-n [])) "empty series")
  (is (= [1010.192347M 1] (stat/sum-n [1010.192347M])) "series with single value")
  (is (= [1.09000001M 3] (stat/sum-n [1.1M -0.01M 0.00000001M])))
  (is (= [6 3] (stat/sum-n (filter even? (range 5)))) "lazy sequence"))

(deftest test-variance-p
  (is (= 0.00M (with-precision 2 :rounding HALF_EVEN (stat/variance-p [0.0M]))))
  (is (= 0.00M (stat/variance-p [0.1M 0.1M 0.1M 0.1M 0.1M])) "series of the same value")
  (is (= 0.06401875M (with-precision 8 :rounding HALF_EVEN
         (stat/variance-p [0.1M, 0.2M, 0.01M, 0.666666667M]))))
  (is (= 0.06401875M (with-precision 8 :rounding HALF_EVEN
                                       (stat/variance-p (filter pos?
                                                                [0.1M -0.1M 0.2M -0.2M 0.01M -0.01M 0.666666667M]))))
      "lazy sequence")
  (is (thrown? IllegalArgumentException (stat/variance-p [])) "empty sequence"))

(deftest test-variance-s
  (is (= 0.00M (stat/variance-s [0.1M 0.1M 0.1M 0.1M 0.1M])) "series of the same value")
  (is (= 0.085358333M (with-precision 8 :rounding HALF_EVEN
         (stat/variance-s [0.1M 0.2M 0.01M 0.666666667M]))))
  (is (= 0.085358333M (with-precision 8 :rounding HALF_EVEN
         (stat/variance-s (filter pos?
                                 [0.1M -0.1M 0.2M -0.2M 0.01M -0.01M 0.666666667M]))))
      "lazy sequence")
  (is (thrown? IllegalArgumentException (stat/variance-s [])) "empty sequence")
  (is (thrown? IllegalArgumentException (stat/variance-s [0.1M])) "series with single element"))

(deftest test-stddev-p
  (is (= 0 (stat/stddev-p [0.0M])))
  (is (= 0 (stat/stddev-p [0.1M 0.1M 0.1M 0.1M 0.1M])) "series of the same value")
  (is (= 0.25301927M (with-precision 16 :rounding HALF_EVEN
          (stat/scaled-decimal 8 (stat/stddev-p [0.1M 0.2M 0.01M 0.666666667M])))))
  (is (= 0.25301927M (with-precision 16 :rounding HALF_EVEN
        (stat/scaled-decimal 8 (stat/stddev-p (filter pos?
                                                      [0.1M -0.1M 0.2M -0.2M 0.01M -0.01M 0.666666667M])))))
      "lazy sequence")
  (is (thrown? IllegalArgumentException (stat/stddev-p [])) "empty sequence"))

(deftest test-stddev-s
  (is (= 0 (stat/stddev-s [0.1M 0.1M 0.1M 0.1M 0.1M])) "series of the same value")
  (is (= 0.29216149M (with-precision 16 :rounding HALF_EVEN
          (stat/scaled-decimal 8 (stat/stddev-s [0.1M 0.2M 0.01M 0.666666667M])))))
  (is (= 0.29216149M (with-precision 16 :rounding HALF_EVEN
        (stat/scaled-decimal 8 (stat/stddev-s (filter pos?
                                                      [0.1M -0.1M 0.2M -0.2M 0.01M -0.01M 0.666666667M])))))
      "lazy sequence")
  (is (thrown? IllegalArgumentException (stat/stddev-s [])) "empty sequence")
  (is (thrown? IllegalArgumentException (stat/stddev-s [0.1M])) "series with single element"))

; arbitrary series for testing covariance
(def series-A [2.82M 1.33M 2.10M -1.75M 4.66M -1.10M 3.98M 2.50M 2.02M 2.14M -0.92M 4.31M])
(def series-B [3.75M 1.93M 2.34M -1.34M 5.09M -2.90M 3.14M 4.60M 3.05M 2.53M -3.46M 4.57M])

(deftest test-covariance-p
  (is (= 5.23926528M (with-precision 16 :rounding HALF_EVEN
          (stat/scaled-decimal 8 (stat/covariance-p series-A series-B))))
      "covariance of two example series with expected calculated by Excel")
  (is (= 4.511532M (with-precision 16 :rounding HALF_EVEN
          (stat/scaled-decimal 6 (stat/covariance-p (take 5 series-A) (take 5 series-B)))))
      "covariance of two lazy sequences with expectected calculated by Excel")
  (is (thrown? IllegalArgumentException (stat/covariance-p [0.1M] [0.3M 0.01M])) "series with different length")
  (is (thrown? IllegalArgumentException (stat/covariance-p [] [])) "empty series")
  (is (= 0.000M (stat/covariance-p [0.1M] [0.01M])) "series with single element"))

(deftest test-covariance-s
  (is (= 5.71556212M (with-precision 16 :rounding HALF_EVEN
          (stat/scaled-decimal 8 (stat/covariance-s series-A series-B))))
      "covariance of two example series with expected calculated by Excel")
  (is (= 5.639415M (with-precision 16 :rounding HALF_EVEN
          (stat/scaled-decimal 6 (stat/covariance-s (take 5 series-A) (take 5 series-B)))))
      "covariance of two lazy sequences with expectected calculated by Excel")
  (is (thrown? IllegalArgumentException (stat/covariance-s [0.1M] [0.3M 0.01M])) "series with different length")
  (is (thrown? IllegalArgumentException (stat/covariance-s [] [])) "empty series")
  (is (thrown? IllegalArgumentException (stat/covariance-s [0.1M] [0.01M])) "series with single element"))