(ns project-euler.problem-3
  (:require [project-euler.algorithms :refer :all]))


(def p3-desc
  "The prime factors of 13195 are 5, 7, 13 and 29. What is the largest prime factor of the number 600851475143?")


(defn p3s0
  []
  (->> 600851475143
       all-factors-of
       (filter prime?)
       (apply max)))
