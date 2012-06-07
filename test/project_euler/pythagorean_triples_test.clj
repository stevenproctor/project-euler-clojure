(ns project-euler.pythagorean-triples-test
  (:use clojure.test
        midje.sweet
        project-euler.core))

(fact
  (pythagorean-triple-for 2 1) => [3 4 5]
  (pythagorean-triple-for 3 1) => [6 8 10]
  (pythagorean-triple-for 3 2) => [5 12 13]
  (pythagorean-triple-for 4 1) => [8 15 17])

