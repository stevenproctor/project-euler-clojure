(ns project-euler.core)

(defn problem1
  []
  (reduce + (filter #(or (zero? (rem % 3)) (zero? (rem % 5))) (range 1000))))

(defn skip-n
  [n s]
  (cond
    (<= n 0) s
    (= n 1) (rest s)
    :else (recur (dec n) (rest s))))

(defn fib
  ([] (concat [0N 1N] (fib 0N 1N)))
  ([a b] (let [c (+ a b)]
          (lazy-seq
            (cons c (fib b c))))))

(defn problem2 []
  (reduce + (filter #(even? %) (take-while #(< % 4000000N) (skip-n 2 (fib))))))

(defn factors-starting-at [f n]
  (cond (= n 1) []
        (zero? (rem n f)) (cons f (factors-starting-at f (/ n f)))
        :else (recur (inc f) n)))

(defn prime-factors-of [n]
  (factors-starting-at 2 n))

(defn problem3 []
  (reduce max (prime-factors-of 600851475143)))
