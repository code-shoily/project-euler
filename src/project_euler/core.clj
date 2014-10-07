(ns project-euler.core)
;; PROBLEMS SOLVED ON 14/8/2013

(defn euler-3 
  [] 
  (apply max 
         (filter prime? 
                 (all-factors-of 600851475143))))

(defn euler-4 
  [] 
  (->> (for [x (range 100 1000) y (range 100 x)] (* x y)) 
       (filter #(= (str %) (apply str (reverse (str %))))) 
       (apply max)))


(defn euler-5 [] (reduce #(lcm %1 %2) 
                          (range 1 (inc 20))))

(defn euler-6 
  [] 
  (- (#(* % %) (apply + (range 1 101))) 
     (apply + (map #(* % %) (range 1 101)))))


(defn euler-7 [] (last (nth-prime 10001)))

(defn turn-into-ints 
      [n] 
      (map #(Integer/parseInt %) 
            (rest (clojure.string/split (clojure.string/replace n "\n" "") #""))))

(def processed-input 
  (turn-into-ints "
    73167176531330624919225119674426574742355349194934
    96983520312774506326239578318016984801869478851843
    85861560789112949495459501737958331952853208805511
    12540698747158523863050715693290963295227443043557
    66896648950445244523161731856403098711121722383113
    62229893423380308135336276614282806444486645238749
    30358907296290491560440772390713810515859307960866
    70172427121883998797908792274921901699720888093776
    65727333001053367881220235421809751254540594752243
    52584907711670556013604839586446706324415722155397
    53697817977846174064955149290862569321978468622482
    83972241375657056057490261407972968652414535100474
    82166370484403199890008895243450658541227588666881
    16427171479924442928230863465674813919123162824586
    17866458359124566529476545682848912883142607690042
    24219022671055626321111109370544217506941658960408
    07198403850962455444362981230987879927244284909188
    84580156166097919133875499200524063689912560717606
    05886116467109405077541002256983155200055935729725
    71636269561882670428252483600823257530420752963450"))

(defn euler-8 
  [] 
  (->> (partition 5 1 processed-input) 
       (map #(apply * %)) (apply max)))

;; I had defined isInteger in the REPL but forgot to work it here.. so.. fuck it!
;(defn euler-9 [] (apply * (first (filter #(= 1000.0 (apply + %)) (filter #(isInteger (last %)) (for [x (range 1 1000) y (range 1 x)] (list x y (Math/sqrt (+ (* x x) (* y y))))))))))

(defn pow 
  [b e] 
  (loop [ctr e out 1] 
        (if (= ctr 0) 
            out 
            (recur (dec ctr) (*' out b)))))

(defn sum-of-digits 
  [n] 
  (apply + 
         (map #(Integer/parseInt %) 
         (-> n str (clojure.string/split #"") rest))))

(defn euler-16 
  [] 
  (sum-of-digits (pow 2 1000)))

(defn euler-20 
  [] 
  (sum-of-digits (fact 100)))

;; First (defn le-numbers "<PASTE THE FUCKING NUMBER>')
(def euler-13 (apply +' 
  (map #(BigInteger. %) 
        (rest (clojure.string/split le-numbers #"\n"))))))
