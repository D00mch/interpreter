(ns dumch.impl.nondeterministic
  (:require [clojure.pprint :refer [pprint]]
            [dumch.environment :as core])
  (:import (dumch.environment Proc)))

(set! *warn-on-reflection* true)

;; analyze

;; returns fn: (env) -> evaluation result
(defprotocol IAnalyze
  (analyze [exp]))

(defn analyze-quoted [[_ quotation]]
  (fn [_ succeed fail] (succeed quotation fail)))

(defn analyze-self-evaluationg [exp]
  (fn [_ succeed fail]
    (succeed exp fail)))

(defn analyze-if [[_ pred conseq alt]]
  (let [pred-fn (analyze pred)
        conseq-fn (analyze conseq)
        alt-fn (analyze alt)]
    (fn [env succeed fail]
      (pred-fn env
               (fn [pred-value fail2]
                 (if (true? pred-value)
                   (when conseq-fn 
                     (conseq-fn env succeed fail2))
                   (when alt-fn 
                     (alt-fn env succeed fail2))))
               fail))))

(defn analyze-sequence [sq]
  (let [sequentially (fn [a b]
                       (fn [env succeed fail]
                         (a env 
                             (fn [_ fail2]
                               (b env succeed fail2))
                             fail)))
        [f & fs] (map analyze sq)]
    (when (nil? f) 
      (throw (ex-info "Empty sequence: analyze" {})))
    (loop [f f
           fs fs]
      (if (seq fs)
        (recur (sequentially f (nth fs 0))
               (next fs))
        f))))

(defn analyze-lambda [params body-sq _name]
  (let [body-fn (analyze-sequence body-sq)]
    (fn [env succeed fail] 
      (succeed (Proc. params body-fn env _name)
               fail))))

(defn analyze-assignment [[_ _name v]]
  (let [val-fn (analyze v)]
    (fn [env succeed fail] 
      (val-fn 
        env
        (fn [_val fail2]
          (let [old-val (core/lookup-variable-value env _name)]
            (succeed (core/set-variable-value! _name _val env)
                     (fn []
                       (core/set-variable-value! _name old-val env)
                       (fail2)))))
        fail))))

(defn analyze-def [[_ _name v]]
  (let [val-fn (analyze v)]
    (fn [env succeed fail] 
      (val-fn env
              (fn [_val fail2]
                (core/define-variable! _name _val env)
                (succeed nil fail2))
              fail))))

(defn analyze-defn [[_ _name params & body]]
  (let [make-proc-fn (analyze-lambda params body _name)]
    (fn [env succeed fail]
      (make-proc-fn env
                    (fn [proc fail2]
                      (core/define-variable! _name proc env)
                      (succeed nil fail2))
                    fail))))

(defn analyze-fn [[_ params & body]]
  (let [make-proc-fn (analyze-lambda params body nil)]
    (fn [env succeed fail] 
      (make-proc-fn env succeed fail))))

(defn get-args [args-fns env succeed fail]
  (if (seq args-fns)
    ((first args-fns)
     env
     (fn [arg fail2]
       (get-args
         (next args-fns)
         env
         (fn [args fail3]
           (succeed (cons arg args) fail3))
         fail2))
     fail)
    (succeed '() fail)))

(defn analyze-let [sexp]
  (analyze (core/let->lambda sexp)))

(defn execute-applicaiton [proc args succeed fail]
  (println :proc proc :args args)
  (cond (core/primitive-procedure? proc) 
        (succeed (apply proc args) fail)

        (core/compound-procedure? proc)
        (let [proc ^Proc proc] 
          ((.body proc)
           (core/extend-env (.params proc) args (.env proc))
           succeed
           fail))
        :else (throw (ex-info  "Unknown procedure type: "  
                              {:proc proc :args args}))))

(defn analyze-application [[op & operands]]
  (let [f-fn (analyze op)
        args-fns (map analyze operands)]
    (fn [env succeed fail]
      (f-fn env
            (fn [proc fail2]
              (get-args
                args-fns
                env
                (fn [args fail3]
                  (execute-applicaiton proc args succeed fail3))
                fail2))
            fail))))

(defn analyze-amb [[_ & amb-choices]]
  (println :amb-choices amb-choices)
  (let [fns (map analyze amb-choices)]
    (fn [env succeed fail]
      (letfn [(try-next [choices]
                (if (seq choices)
                  ((nth choices 0) env
                                   succeed
                                   (fn []
                                     (try-next (next choices))))
                  (fail)))]
        (try-next fns)))))

(extend-protocol IAnalyze
  nil (analyze [s] (analyze-self-evaluationg s))

  java.lang.Boolean (analyze [s] (analyze-self-evaluationg s))
  java.lang.Long (analyze [s] (analyze-self-evaluationg s))
  java.lang.Character (analyze [s] (analyze-self-evaluationg s))
  java.lang.String (analyze [s] (analyze-self-evaluationg s))
  clojure.lang.IPersistentVector 
  (analyze [s] (analyze-self-evaluationg s))

  clojure.lang.Symbol
  (analyze [sexp]
    (fn [env succeed fail]
      (succeed (core/lookup-variable-value env sexp) fail))) 

  clojure.lang.ISeq
  (analyze [[op :as sexp]]
    (case op
      set! (analyze-assignment sexp) 
      amb (analyze-amb sexp)
      def (analyze-def sexp)
      defn (analyze-defn sexp)
      do (analyze-sequence (next sexp))
      fn (analyze-fn sexp)
      if (analyze-if sexp)
      quote (analyze-quoted sexp) 
      let (analyze-let sexp)
      (analyze-application sexp))))

;; Evaluation

(defn ambeval [sexp env succeed fail]
  ((analyze sexp) env succeed fail))

;; Run program

(defn eval-seq [[h & tail :as code] env succeed fail]
  (ambeval  (cons 'do code) env succeed fail))

(defn eval-program
  ([sexps]
   (let [result (atom nil)]
     (eval-program sexps (core/extend-env) result)
     @result))
  ([sexp env result]
   (eval-seq sexp
             env
             (fn [v _] 
               (reset! result v))
             #(println "final fail"))
   @result))

(comment 
  (eval-program '(
                  (let [a (amb 1 2 3)]
                    (if (< a 3) (amb))
                    a
                    )
                  ))

  (eval-program '(
                  (def a (amb 1 2 3))
                  (if (not (> a 2)) (amb))
                  a
                  ))

  (eval-program '(
                  (defn require [p]
                    (if (not p) (amb)))

                  (defn an-element-of [items]
                    (require (some? (seq items)))
                    (amb (first items)
                         (an-element-of (next items))))

                  (def a (an-element-of [1 2 3]))

                  (require (> a 2))

                  a
                  ))
  ,)
