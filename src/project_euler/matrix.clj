(ns project-euler.matrix)

(defn transpose [m]
  (vec (apply map vector m)))

(defn partition-rows [m n step]
  (if (= (count m) 0)
      []
      (concat (partition n step (first m)) (partition-rows (rest m) n step))))

(defn partition-columns [m n step]
  (partition-rows (transpose m) n step))

(defn matrix-get [m r c]
  (nth (nth m r) c))

(defn row [m n]
  (nth m n))

(defn matrix-lines [m f]
  (let [rdim (count m)
        cdim (count (first m))]
    (->> (for [r (range rdim), c (range cdim)]
            {(f rdim cdim r c) [(matrix-get m r c)]})
         (apply merge-with into)
         vals)))

(defn diagonals [m]
  (matrix-lines m (fn [rdim cdim r c] (+ r (- cdim c)))))

(defn reverse-diagonals [m]
  (matrix-lines m (fn [rdim cdim r c] (+ r c))))

(defn partition-diagonals [m n diagonals-fn]
  (apply concat (map #(partition n 1 %) (diagonals-fn m))))

(defn partition-every-diagonal [m n]
  (concat (partition-diagonals m n diagonals) (partition-diagonals m n reverse-diagonals)))
