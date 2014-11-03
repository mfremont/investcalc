(ns
  ^{:author mrf}
  net.mrf.investcalc.stat)

(defn sum-n
  "Computes the sum of the seq series and returns a tuple consisting of the sum and the number of
  elements in the series."
  [series]
  (loop [sum 0, n 0, s series]
    (if (empty? s)
      [sum n]
      (recur (+ sum (first s)) (inc n) (rest s)))))

(defn mean-n
  "Computes the arithmetic mean of a series and returns a tuple consisting of the mean and the
  number of elements in the series."
  [series]
  (if (empty? series)
    [0 0]
    (let [[sum n] (sum-n series)]
      [(/ sum n) n])))

