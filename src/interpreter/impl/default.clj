(ns interpreter.impl.default
  (:refer-clojure :exclude [eval true?] :rename {apply clj-apply})
  (:require [interpreter.type :refer :all])
  (:import [interpreter.type Proc State]))

(declare apply eval)

;; todo: move in common place?
(def ^:private the-global-env (atom {}))

(println the-global-env)

(defn- define-variable!
  ([name val]
   (define-variable! the-global-env name val))
  ([env-atom name val]
   (swap! env-atom #(assoc % name val))))

(defn eval-if [[_ pred consequent alternative] env]
  (if (true? (eval pred env))
    (eval consequent env)
    (if (nil? alternative)
      'ok
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
  (let [evaled-op (eval op env)]
    (apply evaled-op
           (map (fn [operand]
                  (eval operand env))
                operands))))

(defmethod eval-seq 'def
  [[_ & operands] env]
  (let [[name exp] operands
        value (eval exp env)]
    (define-variable! name value))
  'ok)

(defmethod eval-seq 'defn
  [[_ & operands] env]
  (let [[name params body] operands
        new-fn (Proc. params body env name)]
    (define-variable! name new-fn))
  'ok)

(defmethod eval-seq 'if
  [sexp env]
  (eval-if sexp env))

(defmethod eval-seq 'cond
  [sexp env]
  (eval-seq (cond->if sexp) env))

(defmethod eval-seq 'let
  [sexp env]
  (eval-seq (let->fn sexp) env))

(defmethod eval-seq 'fn
  [[op & operands] env]
  (let [[params body] operands]
    (Proc. params
           body
           env
           nil)))

(defn- eval-sexp [sexp env]
  (cond
    (self-evaluating? sexp)
    sexp

    (primitive-procedure-name? sexp)
    (primitive-procedure-map sexp)

    (symbol? sexp)
    (env sexp)

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
  ([sexp] (eval sexp the-global-env))
  ([sexp env]
   (eval-sexp sexp env)))

(defn- next-state [last-state sexp]
  {:deprecated "now"}
  (let [env (:env last-state)]
    (eval-sexp sexp env)))

(defn reduce-state [initial-state sexps]
  {:deprecated "now"}
  (reduce next-state initial-state sexps))

(defmethod eval-seq 'do
  [[_ & operands] env]
  (reduce-state (State. 'NIL env) operands))

(defn eval-program [sexps]
  (:result (reduce-state initial-state sexps)))

;; driver
(def ^:private input-prompt ";;; Input Eval")
(def ^:private output-prompt ";;; Value Eval:")
(def ^:private prompt-for-input #(println %))
(def ^:private announce-output #(println %))

(defn- user-print [input output]
  (println "")
  (println output-prompt))

(defn print-loop
  "reduce-state-fn should be fn, that take State and sexps
  and return State as a result of evaluating sexps"
  [reduce-state-fn]
  (loop [state initial-state]
    (prompt-for-input input-prompt)
    (let [input  (read-string (read-line))
          output (reduce-state-fn state (list input))]
      (user-print (:result output))
      (recur output))))


;(let [input (read-string (read-line))
;      output (eval input the-global-env)]
;  (println output))

(defn print-loop []
  (loop [env the-global-env]
    (prompt-for-input input-prompt)
    (let [input (read-string (read-line))
          output (eval input env)]
      (user-print input output)
      (recur env))))

;(print-loop)
