(ns interpreter.type
  (:refer-clojure :exclude [true?]
                  :rename {apply clj-apply}))

(defrecord State [result env])
(defrecord Proc [params body env name])

(def initial-state (State. 'NIL {}))

(defn map-vals [f m]
  (into (empty m) (for [[k v] m]
                    [k (f v)])))

(defn error [& msg] (assert false (clj-apply str msg)))

(def bools #{'TRUE 'FALSE})

(defn self-evaluating? [sexp]
  (or (number? sexp)
      (bools sexp)))

(defn true? [sexp]
  (not= 'FALSE sexp))

(defn make-if
  ([pred consequence]
   (list 'if pred consequence))
  ([pred consequence alternative]
   (list 'if pred consequence alternative)))

(defn make-fn [params body]
  (list 'fn params body))

(defn pairs->if [[pred consequence & pairs]]
  (if (nil? pairs)
    (make-if pred consequence)
    (make-if pred
             consequence
             (pairs->if pairs))))

(defn cond->if [[_ & pairs]]
  (pairs->if pairs))

(def primitive-procedure-map {'+        +
                              '-        -
                              '*        *
                              '/        /
                              '<        <
                              '>        >
                              '<=       <=
                              '>=       >=
                              'println  println
                              'print    print
                              'not=     not=
                              'quot     quot
                              'even?    even?
                              'odd?     odd?
                              'integer? integer?
                              'string?  string?
                              'symbol?  symbol?
                              'not      'not
                              '=        (fn [& args]
                                          (if (clj-apply = args) 'TRUE 'FALSE))
                              'square   (fn [x] (* x x))
                              })
(def primitive-procedure-name? (set (keys primitive-procedure-map)))
(def primitive-procedure? (set (vals primitive-procedure-map)))
(def compound-procedure? map?)

