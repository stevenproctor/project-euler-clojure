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

(defn diagonals [m]
  (let [rdim (count m)
        cdim (count (first m))]
      (loop [diags []
             roff 0
             coff 0]
          (cond (>= coff cdim) (recur diags (inc roff) 0)
                (>= roff rdim) diags
                :else (recur
                        (concat diags (conj [] (for [i (range 0 (min (- rdim roff) (- cdim coff)))] (matrix-get m (+ roff i) (+ coff i))))) roff (if (zero? roff) (inc coff) cdim))))))

