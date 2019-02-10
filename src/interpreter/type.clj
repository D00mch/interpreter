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

(def primitive-procedure-map {'+      +
                              '-      -
                              '*      *
                              '/      /
                              '=      (fn [& args]
                                        (if (clj-apply = args) 'TRUE 'FALSE))
                              'square (fn [x] (* x x))})
(def primitive-procedure-name? (set (keys primitive-procedure-map)))
(def primitive-procedure? (set (vals primitive-procedure-map)))
(def compound-procedure? map?)

