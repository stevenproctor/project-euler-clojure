(ns project-euler.matrix-test
  (:use midje.sweet
        project-euler.matrix))

(fact
  (transpose [[1 2 3 4]]) => [[1]
                              [2]
                              [3]
                              [4]]
  (transpose [[1 2 3 4]
              [5 6 7 8]]) => [[1 5]
                              [2 6]
                              [3 7]
                              [4 8]]
  (transpose [[1 2 3]
              [4 5 6]
              [7 8 9]]) => [[1 4 7]
                            [2 5 8]
                            [3 6 9]])

(fact
  (partition-rows [[1 2 3 4]] 2 1) => [[1 2] [2 3] [3 4]]
  (partition-rows [[1 2 3 4]
                   [5 6 7 8]] 2 1) => [[1 2] [2 3] [3 4] [5 6] [6 7] [7 8]]
  (partition-rows [[1 2 3]
                   [4 5 6]
                   [7 8 9]] 2 1) => [[1 2] [2 3] [4 5] [5 6] [7 8] [8 9]])

(fact
  (partition-columns [[1 2 3]
                      [4 5 6]
                      [7 8 9]] 2 1) => [[1 4] [4 7] [2 5] [5 8] [3 6] [6 9]])

(fact
  (diagonals [[1 2 3]]) => [[1] [2] [3]]
  (diagonals [[1]
              [2]]) => [[1] [2]]
  (diagonals [[1 2]
              [4 5]]) => [[1 5] [2] [4]]
  (diagonals [[1 2 3]
              [4 5 6]
              [7 8 9]]) => [[1 5 9] [2 6] [3] [4 8] [7]]
  (diagonals [[1 2 3 4]
              [5 6 7 8]]) => [[1 6] [2 7] [3 8] [4] [5]]
  (diagonals [[1 2 3 4]
              [5 6 7 8]
              [9 10 11 12]]) => [[1 6 11] [2 7 12] [3 8] [4] [5 10] [9]])