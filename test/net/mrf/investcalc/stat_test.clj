(ns
  ^{:author mrf
    :doc "unit tests for net.mrf.investcalc.stat"}
  net.mrf.investcalc.stat-test
  (:use clojure.test)
  (:require [net.mrf.investcalc.stat :as stat]))

(deftest test-mean-n
  (is (= [1.1M 3] (stat/mean-n [1.1M 1.1M 1.1M])))
  (is (= [0 0] (stat/mean-n []) "empty series"))
  (is (= [1.1M 1] (stat/mean-n [1.1M]) "series with single value"))
  (is (= [-0.283333333M 6] (stat/mean-n [-1.1M 0.1M 0.1M, 0.1M, -1.1M, 0.2M]
                             "series with non-terminating decimal representation"))))

(deftest test-sum-n
  (is (= [3.3M 3] (stat/sum-n [1.1M 1.1M 1.1M])) "series of the same value")
  (is (= [0 0] (stat/sum-n [])) "empty series")
  (is (= [1010.192347M 1] (stat/sum-n [1010.192347M])) "series with single value")
  (is (= [1.09000001M 3] (stat/sum-n [1.1M -0.01M 0.00000001M]))))