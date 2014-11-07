(ns
  ^{:author mrf}
  net.mrf.investcalc.stat
  (:require [clojure.math.numeric-tower :as math]))

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
    ; sum((x - mean)^2) / n
    (/ (reduce + (map (partial deviation-squared mean) series)) n)
    ))

(defn variance-s
  "Calculates the variance of the sample requested by the seq series."
  [series]
  (let [[mean n] (mean-n series)]
    ; sum((x - mean)^2) / (n -1)
    (/ (reduce + (map (partial deviation-squared mean) series)) (- n 1))
  ))