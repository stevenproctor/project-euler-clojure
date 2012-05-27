(ns project-euler.core
  (:require 
            [clojure.string :as string])
  (:use clojure.math.numeric-tower))

(defn problem1
  ([] (problem1 1000))
  ([n] (reduce + (filter #(or (zero? (rem % 3)) (zero? (rem % 5))) (range n)))))

(defn skip-n
  [n s]
  (cond
    (<= n 0) s
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

(defn problem5
  ([] (problem5 20))
  ([n] (reduce lcm 1 (range 2 (inc n)))))

(defn problem6
  ([] (problem6 100))
  ([n] (bigint (-
         (Math/pow (reduce + (range 1 (inc n))) 2)
         (reduce + (map #(Math/pow % 2) (range 1 (inc n))))))))

(defn is-prime? [n]
  (cond (<= n 1) false
        (= n 2) true
        :else (loop [f 2]
                (cond (zero? (rem n f)) false
                      (> f (Math/sqrt n)) true
                      :else (recur (inc f))))))

(defn problem7
  ([] (problem7 10001))
  ([n] (first (skip-n (dec n) (filter is-prime? (iterate inc 1))))))
