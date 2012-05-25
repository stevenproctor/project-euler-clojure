(ns project-euler.prime-factors-test
  (:use clojure.test
        midje.sweet
        project-euler.core))

(defn mersenne [n]
  (int (dec (Math/pow 2 n))))

(fact
  (prime-factors-of 1) => []
  (prime-factors-of 2) => [2]
  (prime-factors-of 3) => [3]
  (prime-factors-of 4) => [2 2]
  (prime-factors-of 5) => [5]
  (prime-factors-of 6) => [2 3]
  (prime-factors-of 7) => [7]
  (prime-factors-of 8) => [2 2 2]
  (prime-factors-of 9) => [3 3]
  (prime-factors-of 10) => [2 5]
  (prime-factors-of 11) => [11]
  (prime-factors-of 12) => [2 2 3]
  (prime-factors-of 16) => [2 2 2 2]
  (prime-factors-of 25) => [5 5]
  (prime-factors-of (* 2 3 5 7 11 17 37)) => [2 3 5 7 11 17 37]
  (prime-factors-of (mersenne 17)) => [(mersenne 17)])
