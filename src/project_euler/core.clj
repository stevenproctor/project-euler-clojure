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

(defn digits-of [n]
  (map #(Integer/parseInt (str %)) (str n)))

(defn problem8 []
  (let [n 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450]
    (apply max (map
                 #(apply * %) (partition 5 1 (digits-of n))))))
