(ns project-euler.problem-1)

(def p1-desc
  "If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5 below 1000.")

(defn p1s0
  []
  (apply + (filter 
             #(or (zero? (rem % 3))
                  (zero? (rem % 5))) (range 1000))))

(defn p1s1 
  []
  (apply +
    (for [i (range 1000)]
      (if (or (zero? (rem i 3))
              (zero? (rem i 5)))
        i
        0))))
