(ns interpreter.impl.nondeterministic
  (:refer-clojure :exclude [eval true?]
                  :rename {apply clj-apply})
  (:require [interpreter.type :refer :all])
  (:import (interpreter.type State Proc)
           (clojure.lang PersistentList)))

;; deprecated


(declare analyze)

(defn- execute-application
  "proc is "
  [proc args succeed fail]
  (println "inside execute-application")
  (cond
    (primitive-procedure? proc)
    (do
      (println "it's a primitive proc " proc)
      (succeed (clj-apply proc
                          args)
               fail))

    (compound-procedure? proc)
    (do
      (println "it's a compount proc " + proc)
      ((:body proc)
        (merge
          (:env proc)
          {(:name proc) proc}
          (zipmap (:params proc)
                  args))
        succeed
        fail))

    :else
    (error "EXECUTE-APPLICATION FAIL: " proc args)))

;(execute-application '+ '(1 1) #(println %1 %2) #(println 'fail))

(defmulti analyze-seq (fn [exp] (first exp)))

(defn- amb-choices [exp] (rest exp))

(defn amb-try-next [choices env succeed fail]
  (if (empty? choices)
    (fail)
    ((first choices)
      env
      succeed
      (fn [] (amb-try-next (rest choices) env succeed fail)))))

(defmethod analyze-seq 'amb
  [exp]
  (let [cprocs (map analyze (amb-choices exp))]
    (println cprocs)
    (fn [env succeed fail]
      (amb-try-next cprocs env succeed fail))))

(defn- sequentially [a b stop? init]
  (fn [env succeed fail]
    (a env
       (fn [a-value fail2]
         (if (stop? a-value)
           (b env succeed fail2)
           (succeed (State. (not init) env)
                    fail2)))
       fail)))
(defn- analyze-logic [exps stop? init]
  (loop [first-proc (fn [env succeed fail] (succeed (State. init env) fail))
         rest-procs (map analyze exps)]
    (if (empty? rest-procs)
      first-proc
      (recur (sequentially first-proc (first rest-procs) stop? init)
             (rest rest-procs)))))
(defmethod analyze-seq 'and
  [[_ exps]]
  (analyze-logic exps identity true))
(defmethod analyze-seq 'or
  [[_ exps]]
  (analyze-logic exps not false))

(defn- get-args [aprocs env succeed fail]
  (if (empty? aprocs)
    (succeed (State. '() env) fail)
    ((first aprocs)
      env
      ;; succeed for this aproc
      (fn [arg fail2]
        (get-args (rest aprocs)
                  env
                  ;; succeed for get-args
                  (fn [args fail3]
                    (succeed (State. (cons arg args) env)
                             fail3))
                  fail2))
      fail)))
(defmethod analyze-seq :default
  [[op & operands]]
  ;; example (+ 1 2)
  (let [fproc     (analyze op) ; State{+ env}
        arg-procs (map analyze operands)] ;'(State{1 env} State{2 env})
    (fn [env succeed fail]
      (fproc env
             ;; succeed
             (fn [proc fail2]
               (get-args arg-procs
                         env
                         (fn [args fail3]
                           (println "about to invoke ex ap, proc " proc)
                           (execute-application
                             proc args succeed fail3))
                         fail2))
             fail))))

;; todo: rm
;(defmethod analyze-seq :default
;  [[op & operands]]
;  (let [evaled-op (eval op env)]
;    (println "evOp is " evaled-op)
;    (State. (apply evaled-op
;                   (map (fn [operand]
;                          (eval operand env))
;                        operands))
;            env)))
;
;;; todo: remove
;;(def a (analyze '(+ 1 1)))
;(def a (analyze 1))

(get-args (list (analyze 1) (analyze 2))
          {}
          (fn [a b] (println "fucking success " a))
          (fn [] (println "fucking error")))

(a {}
   (fn [a b] (println a))
   (fn [] (println 'e)))

(defmethod analyze-seq 'def
  [[_ name val]]
  (let [vproc (analyze val)]
    (fn [env succeed fail]
      (vproc env
             (fn [val fail2]
               (succeed (State. 'NIL (assoc env name val))
                        fail2))
             fail))))

(defmethod analyze-seq 'defn
  [[_ name params & body]]
  (let [vproc (analyze (make-fn params body))]
    (fn [env succeed fail]
      (vproc env
             (fn [val fail2]
               (succeed (State. 'NIL (assoc env name val))
                        fail2))
             fail))))

(defmethod analyze-seq 'fn
  [_ vars body] ; body
  (let [bproc (analyze-seq body)]
    (fn [env succeed fail]
      (succeed
        (State.
          (Proc. vars bproc env nil)
          env)
        fail))))

(defn analyze [exp]
  (cond
    (number? exp)
    (fn [env succeed fail]
      (succeed
        (State. exp env)
        fail))

    (symbol? exp)
    (fn [env succeed fail]
      (succeed
        (State. (env exp) env)
        fail))

    (primitive-procedure-name? exp)
    (fn [env succeed fail]
      (succeed
        (State. (primitive-procedure-map exp) env)
        fail))

    (seq? exp)
    (analyze-seq exp)

    :else
    (error "ANALYZE FAIL: " exp)))

(defn ambeval
  "succeed is like  (fn [val fail] ...)
   fail    is like  (fn [] ... )"
  [exp env succeed fail]
  ((analyze exp) env succeed fail))


(def ^:private input-prompt ";;; Input Amb-Eval:")
(def ^:private output-prompt ";;; Values Amb-Eval:")
(def ^:private prompt-for-input #(println %))

(defn- announce-output [str]
  (println str))
(defn- user-print [object]
  (println output-prompt object))

(def global-state-atom (atom initial-state))

(declare driver-loop)
(defn- driver-loop-inner [try-again]
  (prompt-for-input input-prompt)
  (let [input (read-string (read-line))]
    (if (= input 'try-again)
      (try-again)
      (do
        (println ";;; starting a new problem")
        (ambeval input ; do we need to wrap it in list?
                 (:env #'global-state-atom)
                 ;; success continuation
                 (fn [output-state next-alternative]
                   (announce-output output-prompt)
                   (user-print (:result output-state))
                   (swap! global-state-atom (fn [_] output-state))
                   (driver-loop-inner next-alternative))
                 ;; failure continuation
                 (fn []
                   (announce-output ";;; there are no more values of ")
                   (user-print input)
                   (driver-loop)))))))
(defn driver-loop []
  (driver-loop-inner
    (fn []
      (println ";;; no current problem")
      (driver-loop))))

;(driver-loop)
;
;;; example, todo: remove
;(defn- print-loop
;  "reduce-state-fn should be fn, that take State and sexps
;  and return State as a result of evaluating sexps"
;  [reduce-state-fn]
;  (loop [state initial-state]
;    (prompt-for-input input-prompt)
;    (let [input  (read-string (read-line))
;          output (reduce-state-fn state (list input))]
;      (user-print (:result output))
;      (recur output))))
;
;(def new-withdraw
;  (let [balance (atom 100)]
;    (fn [amount]
;      (swap! balance (fn [prev] amount))
;      (deref balance))))
;(new-withdraw 5)