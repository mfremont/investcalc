(ns
  ^{:author mrf}
  net.mrf.investcalc.stat
  (:require [clojure.math.numeric-tower :as math]))

(defn scaled-decimal [scale n]
  ; TODO: default *math-context*
  (.setScale (bigdec n) scale (.getRoundingMode *math-context*)))

(defn sum-n
  "Calculates the sum of the seq series and returns a tuple consisting of the sum and the number of
  elements in the series."
  [series]
  (loop [sum 0, n 0, s series]
    (if (empty? s)
      [sum n]
      (recur (+ sum (first s)) (inc n) (rest s)))))

(defn mean-n
  "Calculates the arithmetic mean of the seq series and returns a tuple consisting of the mean and the
  number of elements in the series. If the values in the series are precision decimals, the
  with-precision macro can be used to wrap the call to avoid non-terminating decimal expansion
  exceptions."
  [series]
  (if (empty? series)
    [0 0]
    (let [[sum n] (sum-n series)]
      [(/ sum n) n])))

(defn deviation-squared [mean x]
  "Calculates the square of the deviation of x from the mean."
  (math/expt (- x mean) 2))

(defn variance-p
  "Calculates the variance of the population represented by the seq series."
  [series]
  (let [[mean n] (mean-n series)]
    (if (> n 0)
      ; sum((x - mean)^2) / n
      (/ (reduce + (map (partial deviation-squared mean) series)) n)
      (throw (IllegalArgumentException. "variance-p can only be calculated for a sequence of one or more elements")))))

(defn variance-s
  "Calculates the variance of the sample represented by the seq series."
  [series]
  (let [[mean n] (mean-n series)]
    (if (> n 1)
      ; sum((x - mean)^2) / (n -1)
      (/ (reduce + (map (partial deviation-squared mean) series)) (- n 1))
      (throw (IllegalArgumentException. "variance-s can only be calculated for a sequence of two or more elements")))))

(defn stddev-p
  "Calculates the standard deviation of the population represented by the seq series."
  [series]
  (math/sqrt (variance-p series)))

(defn stddev-s
  "Calculates the standard deviation of the sample represented by the seq series."
  [series]
  (math/sqrt (variance-s series)))

(defn covariance-p
  "Calculates the covariance of the popluations represented by the seqs A and B."
  [a b]
  (let [[mean-a n-a] (mean-n a)
        [mean-b n-b] (mean-n b)
        deviation-a (fn [x] (- x mean-a))
        deviation-b (fn [x] (- x mean-b))]
    (cond
      (not (= n-a n-b)) (throw (IllegalArgumentException. "covariance can only be calculated if series are same length"))
      (= 0 n-a) (throw (IllegalArgumentException. "covariance can only be calculated if for series with one or more elements"))
      ; sum(deviation(a) * deviation(b)) / n
      :else (/ (reduce + (map * (map deviation-a a) (map deviation-b b)))
               n-a))))

(defn covariance-s
  "Calculates the covariance of the samples represented by the seqs A and B."
  [a b]
  (let [[mean-a n-a] (mean-n a)
        [mean-b n-b] (mean-n b)
        deviation-a (fn [x] (- x mean-a))
        deviation-b (fn [x] (- x mean-b))]
    (cond
      (not (= n-a n-b)) (throw (IllegalArgumentException. "covariance can only be calculated if series are same length"))
      (< n-a 2) (throw (IllegalArgumentException. "covariance can only be calculated if for series with two or more elements"))
      ; sum(deviation(a) * deviation(b)) / (n - 1)
      :else (/ (reduce + (map * (map deviation-a a) (map deviation-b b)))
               (- n-a 1)))))