(ns polynomial-solve)


(defn factorial [n]
  (reduce * (map bigint (range 1 (inc n)))))

(defn h [n m]
  (let [mm (inc m)]
    (map (fn [x] (vec (for [i (reverse (range n))] (mod (int (/ x (Math/round (Math/pow mm i)))) mm))))
         (range (Math/round (Math/pow mm n))))))

(defn H [n m]
  (mapcat (group-by #(apply max %) (h n m)) (range m)))

(def cached-H (memoize H))

(defn G [M]
  (reduce + 1 (map-indexed (fn [i m] (* (inc i) m)) M)))

(defn G' [M]
  (reduce + 1 (map-indexed (fn [i m] (* (+ i 2) m)) M)))

(def cached-G (memoize G))
(def cached-G' (memoize G'))

(defn P [M]
  (/ (factorial (dec (G' M))) (* (factorial (G M)) (reduce * (map factorial M)))))

(def cached-P (memoize P))

(defn K-old [C M]
  (/ (.pow (bigdec (first C)) (int (G M))) (.pow (bigdec (second C)) (int (G' M)))))

(defn K [C M]
  (/ (Math/pow (first C) (cached-G M)) (Math/pow (second C) (int (cached-G' M)))))

(defn R [C M]
  (reduce * (map #(.pow (bigdec %1) (int %2)) (drop 2 C) M)))

(def solve-depth 4)
(def solve-limit 100)

(defn limited-M-impl [n]
  (loop [i 1]
    (let [Ms (cached-H n i)]
      (if (> (count Ms) solve-limit)
        (vec (take solve-limit Ms))
        (recur (inc i))))))

(def limited-M (memoize limited-M-impl))

(comment (mapv limited-M (range 1 15)))

(defn solve-polynomial
  ([p] (solve-polynomial p solve-depth))
  ([p solve-depth]
   (let [C (update (vec p) 1 #(* -1 %))]
     (for [M (limited-M (- (count p) 2))]
       (* (cached-P M) (K C M) (R C M))))))

(defn eval-polynomial [p x]
  (reduce + (map-indexed #(* (.pow (bigdec x) (int %1)) %2) p)))

(defn binomial [n k]
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

(defn binomial-long [n k]
  (long (binomial n k)))

(def binomial-fast (memoize binomial-long))

(defn shifted-coeffs [coeffs a]
  (let [n (dec (count coeffs))]
    (vec
     (for [j (range (inc n))]
       (reduce + (for [k (range j (inc n))]
                   (*' (nth coeffs k)
                       (binomial k j)
                       (.pow (bigdec a) (- k j)))))))))

(defn shifted-coeffs-gen [coeffs]
  (let [n (dec (count coeffs))]
    `(fn [~'b]
       (let [~'a (bigdec ~'b)
             ~'a-pow (vec (take ~n (iterate (fn [~'e] (*' ~'a ~'e)) ~'a)))]
         ~(vec
          (for [j (range (inc n))]
            (cons `+ (for [k (range j (inc n))]
                       (cond
                         (= 0 (- k j)) (*' (nth coeffs k) (binomial k j))
                         :else
                         `(*'
                           ~(*' (nth coeffs k) (binomial k j))
                           ~@(cond (= 1 (- k j)) ['a]
                                   :else `[(nth ~'a-pow ~(dec (- k j)))])))))))))))

(def eps (/ 1 (.pow 2M 25)))

(defn polynomial-zero-old [p]
  (let [initial-try (solve-polynomial p)
        initial-sum (reduce + initial-try)]
    (if (< (eval-polynomial p initial-sum) eps)
      (reduce + (solve-polynomial p 100))
      (loop [guess (first initial-try) i 0]
        (let [g (shifted-coeffs p guess)
              new-try (solve-polynomial g)
              convergent? (< (apply max (drop-last (sort (map abs new-try)))) 5.0E-1)
              sum (reduce + new-try)]
          (if (> i 5000)
            :timeout
            (if (or (and convergent? (< (abs sum) eps)) (< (abs (eval-polynomial p (+ guess sum))) eps))
              (do
                (println "took " (inc i) " iterations")
                (+ guess sum))
              (recur (if convergent? (+ guess sum) (+ guess (first new-try))) (inc i)))))))))

(defn polynomial-zero-v2 [p]
  (loop [guess (first (solve-polynomial p)) i 0]
    (let [new-try (solve-polynomial (shifted-coeffs p guess))
          sum (reduce + new-try)
          convergent? (< (abs sum) 1.0)
          new-guess (+ guess (if convergent? sum (first new-try)))]
      (cond
        (> i 500) :timeout
        (< (abs (eval-polynomial p new-guess)) eps) (do (println "took " (inc i) " iterations") new-guess)
        :else (recur new-guess (inc i))))))

(def sum-eps (float (/ 1 (.pow 2M 15))))

(defn polynomial-zero-v3 [p]
  (loop [guess (first (solve-polynomial p)) i 0]
    (let [new-try (solve-polynomial (shifted-coeffs p guess))
          sum (reduce + new-try)
          convergent? (< (abs sum) 1.0)
          new-guess (+ guess (if convergent? sum (first new-try)))]
      (cond
        (> i 500) :timeout
        (< (abs sum) sum-eps) (do (println "took " (inc i) " iterations") new-guess)
        :else (recur new-guess (inc i))))))

(defn polynomial-zero [p]
  (let [coeff-shift-fn (eval (shifted-coeffs-gen p))]
      (loop [guess (first (solve-polynomial p)) i 0]
        (let [new-try (solve-polynomial (coeff-shift-fn guess))
           sum (reduce + new-try)
           convergent? (< (abs sum) 1.0)
           new-guess (+ guess (if convergent? sum (first new-try)))]
       (cond
         (> i 500) {:best-guess new-guess :abort-reason :timeout}
         (and (< (abs sum) sum-eps) (< (abs (eval-polynomial p new-guess)) eps)) (do (println "took " (inc i) " iterations") new-guess)
         :else (recur new-guess (inc i)))))))



(comment
  (< 2.2E-5 sum-eps)

  (first (solve-polynomial (shifted-coeffs [13 2 -3 4 -5 6 7] (+ -6.5 1.0477744413637409 0.8699702812874726 0.7207215696978816 0.5948073611923806 0.4876545161033754 0.39513541321267354))))

  (reduce + (solve-polynomial (shifted-coeffs [13 2 -3 4 -5 6 7] (+ -6.5 1.0477744413637409 0.8699702812874726 0.7207215696978816 0.5948073611923806 0.4876545161033754 0.39513541321267354 0.7133359236528278 0.14214387068292733 0.007611027272194282 4.048926740195591E-9))))

  (eval-polynomial [13 2 -3 4 -5 6 7] -1.5208455914855992)

  (eval-polynomial [1 2 -30 4 -5 6 700] (polynomial-zero [1 2 -30 4 -5 6 700]))

  (time (polynomial-zero [3.3937279345 2 -30 4 -5 6 700]))

  (let [p [3.3937279345 2 -30 4 -5 6 700]
        zero (polynomial-zero p)]
    (eval-polynomial p (+ (* 31 eps) (/ eps 2) (/ eps 4) ) )
    )

  2.959408762492899


  (time (polynomial-zero-old [3.3937279345 2 -30 4 -5 6 700]))

  (reduce + (solve-polynomial (shifted-coeffs [3.3937279345 2 -30 4 -5 6 700] (polynomial-zero [3.3937279345 2 -30 4 -5 6 700]))))

  (shifted-coeffs-gen [3.3937279345 2 -30 4 -5 6 700])

  (shifted-coeffs [3.3937279345 2 -30 4 -5 6 700] 5)

  ((eval (shifted-coeffs-gen [3.3937279345 2 -30 4 -5 6 700])) 5)

  (*' (bigdec 0.5) (bigdec 0.5))


  (eval-polynomial [3.3937279345 2 -30 4 -5 6 700] (+ 0.04705 (polynomial-zero [3.3937279345 2 -30 4 -5 6 700])))

  (eval-polynomial [3.3937279345 2 -30 4 -5 6 700] (:best-guess (polynomial-zero [3.3937279345 2 -30 4 -5 6 700])))

  (eval-polynomial [3.3937279345 2 -30 4 -5 6 700] (polynomial-zero-v3 [3.3937279345 2 -30 4 -5 6 700]))

  (solve-polynomial [13 2 -3 4 -5 6 7])


  (sort [1 2 3 4 5 6])

  (solve-polynomial (shifted-coeffs [13 2 -3 4 -5 6 7] -1.520845591531238))

  (time (shifted-coeffs [13 2 -3 4 -5 6 7] -1.520845591531238))

  (eval-polynomial [-5 1 1] (polynomial-zero [-5 1 1]))

  (shifted-coeffs [1 2 3] 2)

  (shifted-coeffs-gen [1 2 3])

  (G' [0])

  (factorial (dec (G' [0])))

  (P [1 2 3])

  (R [1 2 3 4] [1 2 3 4 5 6])

  (H 1 4)

  (P [3])

  (K [-2 5 2] [3])

  (G' [0])

  (Math/pow 0 7)

  (float (reduce + (solve-polynomial [6.3 -5 1])))

  (solve-polynomial [-5 1 1])

  (reduce + (solve-polynomial (shifted-coeffs [-5 1 1] 5)))

  (reduce + (solve-polynomial (shifted-coeffs [-5 1 1] (+ 5 -3.0945 -0.11421211874086255 -3.3781217496705986E-8 3.294023596864004E-18))))

  (reduce + (solve-polynomial (shifted-coeffs [-5 1 1] -3.0945)))

  (reduce + (solve-polynomial (shifted-coeffs [-5 1 1] (+ -3.0945 0.30320483279093896))))

  (reduce + (solve-polynomial (shifted-coeffs [-5 1 1] (+ -3.0945 0.30320483279093896 7.319731140996705E-6))))

  (reduce + (solve-polynomial (shifted-coeffs [-5 1 1] 2.866555210199253)))


  (first (solve-polynomial (shifted-coeffs [-5 1 1] (first (solve-polynomial [-5 1 1])))))


  (eval-polynomial [6.3 -50 90] (reduce + (solve-polynomial [6.3 -50 90] 1000)))

  (reduce + (solve-polynomial [6.3 -50 90] 1000))

  (eval-polynomial [6.3 -50 90] 0.19315808)

  (Math/pow 4 3)

  )
