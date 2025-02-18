(ns )


(defn- rotations
  "Returns a lazy seq of all rotations of a seq"
  [x]
  (if (seq x)
    (map
     (fn [n _]
       (lazy-cat (drop n x) (take n x)))
     (iterate inc 0) x)
    (list nil)))

(defn- bwt [s]
  (->> (apply map str (rotations (str s "!")))
       (sort)
      (map last)
      (apply str)))

(defn- ivbwt [s]
  (loop [l (map str s)]
    (if (< (count (first l)) (count s))
      (recur (map #(str (first %) (second %)) (partition 2 (interleave s (sort l)))))
      (subs (first (filter #(= (last %) \!) l)) 0 (dec (count s))))))

(defn- rle [s]
  (#(if (Character/isDigit ^char (first %)) % (str 1 %)) (apply str (map #(if (> (count %) 1) (str (count %) (first %)) (str (first %))) (partition-by identity s)))))

(defn- ivrle [s]
  (apply str (map (fn [[n [c & r]]] (apply str (.repeat (str c) (Integer/parseInt (apply str n))) r)) (partition 2 (partition-by #(Character/isDigit ^char %) s)))))

(def ^{:private true} common-type-substitutions
  {'java.lang.String "%Z"
   'java.lang.Byte "%B"
   'java.lang.Character "%C"
   'java.lang.Short "%S"
   'java.lang.Integer "%I"
   'java.lang.Long "%L"
   'java.lang.Float "%F"
   'java.lang.Double "%D"
   'byte "%b"
   'char "%c"
   'short "%s"
   'int "%i"
   'long "%l"
   'float "%f"
   'double "%d"})

(defn mangle-name [fn-name argument-types]
  (->>
   argument-types
   (map common-type-substitutions)
   (map str)
   (map #(.replace ^String % "!" "%E"))
   (clojure.string/join "%%")
   (bwt)
   (rle)
   (str (name fn-name) "-^")))

(defn unmangle-name [fn-name]
  (let [[base-name mangled-name] (clojure.string/split fn-name #"-\^")]
    [(symbol base-name)
     (->>
      (->
       mangled-name
       (ivrle)
       (ivbwt)
       (clojure.string/split #"%%"))
      (map #(.replace ^String % "%E" "!"))
      (map (clojure.set/map-invert common-type-substitutions))
      (vec))]))

(defn- get-typehints [args]
  (vec (map (fn [v] (if (instance? clojure.lang.IMeta v) (:tag (meta v)) (symbol (.getName (class v))))) args)))

(defn- fqns [sym]
  (str (get (ns-aliases *ns*) (symbol (namespace sym)) (symbol (namespace sym)))))

(defmacro call-static-fn [fn-name args]
  (let [static-fn-name ((:impls @@(find-var (symbol (fqns fn-name) (str "__static-function-info-" (name fn-name))))) (get-typehints args))]
    `(~static-fn-name ~@args)))

(defmacro declare-static-fn
  "works similar to `defprotocol` but only for one function and it defines a macro instead of functions. It will emit the actual function based on the annotated type, shifting the dispatch to macroexpansion-time, resulting in `invokestatic` bytecode ops for better performance."
  {:style/indent [:defn]}
  [fn-name args]
  (let [proto-name (symbol (str "__static-function-info-" fn-name))]
    `(do
       (def ~proto-name (atom {:impls {}}))
       (defmacro ~fn-name ~args (list `call-static-fn '~(symbol (str *ns*) (str fn-name)) ~args)))))

(defmacro implement-static
  {:style/indent [:defn]}
  [fn-name args body]
  (let [proto-name (symbol (str "__static-function-info-" (name fn-name)))
        proto-fqn (if (namespace fn-name) (symbol (fqns fn-name) (str proto-name)) (symbol (str *ns*) (str proto-name)))
        argument-types (get-typehints args)
        proto-atom @(find-var proto-fqn)
        new-fn-name (mangle-name fn-name argument-types)
        _ (swap! proto-atom assoc-in [:impls argument-types] (symbol (str *ns*) new-fn-name))]
    `(defn ~(symbol new-fn-name) ~args ~body)))

(defn- get-static-fn
  "retrieves the namespaced symbol for an implementation of a static function."
  [static-fn argument-types]
  (let [namespaced-fn-name (if (namespace static-fn) static-fn (symbol (str *ns*) (str static-fn)))]
    ((:impls @@(find-var (symbol (fqns namespaced-fn-name) (str "__static-function-info-" (name namespaced-fn-name))))) argument-types)))

(defmacro sfn-invoke
  [sfn-call-expr alt-expr]
  (let [[sfn & args] sfn-call-expr]
    (if (get-static-fn sfn (get-typehints args))
       SFN-CALL-EXPR
       ALT-EXPR)))
