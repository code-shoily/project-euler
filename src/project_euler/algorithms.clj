(ns project-euler.algorithms)


(def fibs 
  (lazy-cat 
    [0 1] 
    (map + (rest fibs) fibs)))


(defn prime? 
  [n] 
  (if (or (<= n 2) (zero? (mod n 2))) 
    (= n 2) 
    (not (reduce #(or %1 %2) 
                 false 
                 (for [i (range 3 (inc (Math/sqrt n)) 2)] 
                   (zero? (mod n i)))))))

(defn nth-prime 
  [^long n] 
  (->> (filter prime? (iterate inc 2)) (take n)))


(defn lazy-primes 
  []
  (letfn [(enqueue [sieve n step] 
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve candidate]
            (if-let [step (sieve candidate)]
              (-> sieve
                (dissoc candidate)
                (enqueue candidate step))
              (enqueue sieve candidate (+ candidate candidate))))
          (next-primes [sieve candidate]
            (if (sieve candidate)
              (recur (next-sieve sieve candidate) (+ candidate 2))
              (cons candidate (lazy-seq 
                                (next-primes (next-sieve sieve candidate) 
                                             (+ candidate 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))


(defn all-factors-of 
  [^long n] 
  (filter #(zero? (rem n %)) (range 1 (Math/sqrt n) 2)))

(defn gcd 
  [a b] 
  (if (zero? b) a (recur b (mod a b))))

(defn lcm [a b] (/ (* a b) (gcd a b)))

(defn factorial [n] (apply *' (range 1 (inc n))))
