(ns interpreter.impl.default
  (:refer-clojure :exclude [eval true?]
                  :rename {apply clj-apply})
  (:require [interpreter.type :refer :all])
  (:import (interpreter.type State Proc)))

(declare apply eval)

(defn eval-if [[_ pred consequent alternative] env]
  (if (true? (eval pred env))
    (eval consequent env)
    (if (nil? alternative)
      'NIL
      (eval alternative env))))

(defn let->fn [[_ bindings body]]
  (let [params (take-nth 2 bindings)
        args   (take-nth 2 (rest bindings))]
    (cons
      (make-fn params body)
      args)))

(defn eval-let [[bindings body] env]
  (eval body (merge env
                    (map-vals #(eval % env) (clj-apply hash-map bindings)))))

(defmulti eval-seq (fn [sexp env] (first sexp)))

(defmethod eval-seq :default
  [[op & operands] env]
  (State. (apply (eval op env)
                 (map (fn [operand]
                        (eval operand env))
                      operands))
          env))

(defmethod eval-seq 'def
  [[_ & operands] env]
  (State. 'NIL
          (let [[name exp] operands
                value (eval exp env)]
            (assoc env name value))))

(defmethod eval-seq 'defn
  [[_ & operands] env]
  (State. 'NIL
          (let [[name params body] operands
                new-fn (Proc. params body env name)]
            (assoc env name new-fn))))

(defmethod eval-seq 'if
  [sexp env]
  (State. (eval-if sexp env) env))

(defmethod eval-seq 'cond
  [sexp env]
  (eval-seq (cond->if sexp) env))

(defmethod eval-seq 'let
  [sexp env]
  (eval-seq (let->fn sexp) env))

(defmethod eval-seq 'fn
  [[op & operands] env]
  (let [[params body] operands]
    (State. (Proc. params
                   body
                   env
                   nil)
            env)))

(defn- eval-sexp [sexp env]
  (cond
    (self-evaluating? sexp)
    (State. sexp env)

    (primitive-procedure-name? sexp)
    (State. (primitive-procedure-map sexp) env)

    (symbol? sexp)
    (State. (env sexp) env)

    (seq? sexp)
    (eval-seq sexp env)

    :else
    (error "EVAL FAIL: " sexp)))

(defn- apply [proc args]
  (cond
    (primitive-procedure? proc)
    (clj-apply proc args)

    (compound-procedure? proc)
    (eval (:body proc)
          (merge
            (:env proc)
            {(:name proc) proc}
            (zipmap (:params proc)
                    args)))

    :else
    (error "APPLY FAIL: " proc args)))

(defn- eval
  ([sexp] (eval sexp {}))
  ([sexp env]
   (:result (eval-sexp sexp env))))

(defn- next-state [last-state sexp]
  (let [env (:env last-state)]
    (eval-sexp sexp env)))

(defn reduce-state [initial-state sexps]
  (reduce next-state initial-state sexps))

(defmethod eval-seq 'do
  [[_ & operands] env]
  (reduce-state (State. 'NIL env) operands))

(defn eval-program [sexps]
  (:result (reduce-state initial-state sexps)))