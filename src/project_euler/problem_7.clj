(ns project-euler.problem-7
  (:require [project-euler.algorithms :refer :all]))

(def warning true)

(def p7-desc
  "By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13. What is the 10 001st prime number?")

(defn p7s0 [] (-> 10001 nth-prime last))
