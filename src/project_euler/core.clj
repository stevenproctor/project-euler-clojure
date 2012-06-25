(ns project-euler.core
  (:require 
            [clojure.string :as string])
  (:use clojure.math.numeric-tower
        clojure.set
        project-euler.matrix))

(defn factor-of? [f n]
  (zero? (rem n f)))

(defn square [n]
  (* n n))

(defn sum [s]
  (reduce + s))

(defn multiply [s]
  (reduce * s))

(defn make-sorted-set [coll]
  (into (sorted-set) coll))

(defn wrap-at [n coll]
  (let [[f l] (split-at n coll)]
    (concat l f)))

(defn to-vectors [coll]
  (map vec coll))

(defn primes-under [n]
    (loop [sieve (set (cons 2 (range 3 n 2)))
           f 3]
      (if (> (square f) n)
        sieve
        (recur (difference sieve (range (square f) n f)) (inc f)))))

(defn primes-under2 [n]
  (let [sieve (transient (set (cons 2 (range 3 n 2))))]
    (loop[s sieve
           f 3]
      (cond (> (square f) n) (persistent! s)
            :else (recur (reduce disj! s (range (square f) n f)) (inc f))))))

(defn prime? [n] (contains? (primes-under2 (inc n)) n))

(defn problem1
  ([] (problem1 1000))
  ([n] (sum (filter #(or (factor-of? 3 %) (factor-of? 5 %))) (range n))))

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
  (sum (filter #(even? %) (take-while #(< % 4000000N) (skip-n 2 (fib))))))

(defn factors-starting-at [f n]
  (cond (= n 1) []
        (factor-of? f n) (cons f (factors-starting-at f (/ n f)))
        :else (recur (inc f) n)))

(defn prime-factors-of [n]
  (factors-starting-at 2 n))

(defn factors-count [n]
  (multiply (map #(inc %) (vals (frequencies (prime-factors-of n))))))

(defn problem3
  ([] (problem3 600851475143))
  ([n] (reduce max (prime-factors-of n))))

(defn palindrome? [s]
  (= (str s) (string/join (reverse (str s)))))

(defn problem4 []
  (apply max (filter palindrome? (for [x (range 100 1000) y (range 100 1000)] (* x y)))))

(defn problem5
  ([] (problem5 20))
  ([n] (reduce lcm 1 (range 2 (inc n)))))

(defn problem6
  ([] (problem6 100))
  ([n] (bigint (-
         (square (sum (range 1 (inc n))))
         (sum (map #(square %) (range 1 (inc n))))))))

(defn prime? [n]
  (cond (<= n 1) false
        (= n 2) true
        :else (loop [f 2]
                (cond (factor-of? f n) false
                      (> f (Math/sqrt n)) true
                      :else (recur (inc f))))))

(defn problem7
  ([] (problem7 10001))
  ([n] (first (skip-n (dec n) (filter prime? (iterate inc 1))))))

(defn digits-of [n]
  (map #(Integer/parseInt (str %)) (str n)))

(defn problem8 []
  (let [n 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450]
    (apply max (map
                 #(apply * %) (partition 5 1 (digits-of n))))))

(defn pythagorean-triple-for [m n]
  (let [mm (square m)
        nn (square n)]
        (sort [(int (- mm nn)) (* 2 m n) (int (+ mm nn))])))

(defn pythagorean-triples
  ([] (cons (pythagorean-triple-for 2 1) (pythagorean-triples 2 2)))
  ([m n]
      (if (< n m)
        (lazy-seq (cons (pythagorean-triple-for m n) (pythagorean-triples m (inc n))))
        (recur (inc m) 1))))

(defn problem9 []
   (apply * (first (filter #(= (sum %) 1000) (pythagorean-triples)))))

(defn problem10
  ([] (problem10 2000000))
  ([n] (bigint (sum (primes-under2 n)))))

(defn problem11
  []
  (let [m [[ 8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8]
           [49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0]
           [81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65]
           [52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91]
           [22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80]
           [24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50]
           [32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70]
           [67 26 20 68  2 62 12 20 95 63 94 39 63  8 4  91 66 49 94 21]
           [24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72]
           [21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95]
           [78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92]
           [16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57]
           [86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58]
           [19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40]
           [ 4 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66]
           [88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69]
           [ 4 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36]
           [20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16]
           [20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54]
           [ 1 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48]]]
    (apply max
           (map multiply
                (concat (partition-rows m 4 1)
                        (partition-columns m 4 1)
                        (partition-every-diagonal m 4))))))

(defn triangle-numbers
  ([] (concat [1] (triangle-numbers 1 2)))
  ([s n] (let [t (+ s n)]
          (lazy-seq
            (cons t (triangle-numbers t (inc n)))))))

(defn problem12 []
  (first (drop-while #(<= (factors-count %) 500) (triangle-numbers))))
