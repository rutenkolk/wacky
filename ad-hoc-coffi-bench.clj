(ns )


  (require '[criterium.core :as crit])

  (->>
   (with-out-str
     (eval
      (let [pow2 #(Math/round (Math/pow 2 %))
                                        ;testobj (ArrayTestType. 5 6 (int-array (repeatedly m (rand-int 2000000))))
            start 0
            end 18
            m (* 2 (- end start))
            c (atom 1)
            ]

        (concat
         [`do]
         (map
          (fn [n]
            (list
             `do
             (list `.println `*err* (str "Benchmarking ArrayTestType" n " 2^" (dec (+ start @c)) " raw? false copy-method: " array-copy-method " (" @c "/" m ")"))
             (swap! c inc)
             (list `defstruct (symbol (str "ArrayTestType" n)) ['z [::array ::int n :raw? false]])
             (list `println (.repeat "-" 80))
             (list `println (str "Benchmarking ArrayTestType" n " raw? false copy-method: " array-copy-method))
             (list `with-open ['arena `(confined-arena)]

                   (list `let ['obj (list (symbol (str "ArrayTestType" n ".")) (list `vec (list `int-array n (list `repeatedly n `(fn [] (rand-int 2000000))))))
                               'segment (list `alloc-instance (keyword (str *ns*) (str "ArrayTestType" n)) 'arena)
                               '_ (list `serialize-into 'obj (keyword (str *ns*) (str "ArrayTestType" n)) 'segment nil)

                               ]
                         (list `crit/bench
                               #_(list `-> 'obj (list `serialize (keyword (str *ns*) (str "ArrayTestType" n))) (list `deserialize (keyword (str *ns*) (str "ArrayTestType" n))))
                               #_(list `serialize-into 'obj (keyword (str *ns*) (str "ArrayTestType" n)) 'segment nil)
                               (list `deserialize-from 'segment (keyword (str *ns*) (str "ArrayTestType" n)))
                               )
                         ))
             (list `println (.repeat "-" 80))))
          (map pow2 (range start end)))

         (map
          (fn [n]
            (list
             `do
             (list `.println `*err* (str "Benchmarking ArrayTestType" n " raw? true copy-method: " array-copy-method " (" @c "/" m ")"))
             (swap! c inc)
             (list `defstruct (symbol (str "ArrayTestType" n)) [[::array ::int n :raw? true] 'z])
             (list `println (.repeat "-" 80))
             (list `println (str "Benchmarking ArrayTestType" n " raw? true copy-method: " array-copy-method))
             (list `with-open ['arena `(confined-arena)]
                   (list `let ['obj (list (symbol (str "ArrayTestType" n ".")) (list `int-array n (list `repeatedly n `(fn [] (rand-int 2000000)))))
                               'segment (list `alloc-instance (keyword (str *ns*) (str "ArrayTestType" n)) 'arena)
                               '_ (list `serialize-into 'obj (keyword (str *ns*) (str "ArrayTestType" n)) 'segment nil)
                               ]
                         (list `crit/quick-bench
                               #_(list `-> 'obj (list `serialize (keyword (str *ns*) (str "ArrayTestType" n))) (list `deserialize (keyword (str *ns*) (str "ArrayTestType" n))))
                               (list `serialize-into 'obj (keyword (str *ns*) (str "ArrayTestType" n)) 'segment nil)
                               #_(list `deserialize-from 'segment (keyword (str *ns*) (str "ArrayTestType" n)))
                               )
                         ))
             (list `println (.repeat "-" 80))))
          (map pow2 (range start end)))

         [(list `.println `*err* "done")]
         ))))
   (re-seq #"Benchmarking.*?(.*?ArrayTestType.*?) raw\? (.*?) copy-method: (.*)\r\n.*\r\n.*Execution time mean : (\d+?,\d+?.*?s)\n.*Execution time std-deviation : (\d+?,\d+?.*?s)")
   (map rest)
   (map vec)
   (vec)
   )


  (doall (map println (map #(clojure.string/replace % "." ",") (map pr-str (map (fn [[x u]] (case u "ms" (* 1000000 x) "µs" (* 1000 x) x)) (map #(update % 0 (fn [x] (Double/parseDouble (clojure.string/replace x "," ".")))) (map #(clojure.string/split % #" ") (map #(nth % 1) bench-struct-with-many-members))))))))
