(ns project-euler.core
  (:require [clojure.string :as string]))

(defn problem1
  ([] (problem1 1000))
  ([n] (reduce + (filter #(or (zero? (rem % 3)) (zero? (rem % 5))) (range n)))))

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

(defn problem3
  ([] (problem3 600851475143))
  ([n] (reduce max (prime-factors-of n))))

(defn is-palindrome? [s]
  (= (str s) (string/join (reverse (str s)))))

(defn problem4 []
  (apply max (filter is-palindrome? (for [x (range 100 1000) y (range 100 1000)] (* x y)))))

