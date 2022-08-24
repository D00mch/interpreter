(ns dumch.impl.direct
  (:require [dumch.environment :as core])
  (:import (dumch.environment Proc)))

(defn execute-applicaiton [proc args]
  (cond (core/primitive-procedure? proc) (apply proc args)
        (core/compound-procedure? proc)
        (let [proc ^Proc proc] 
          ((.body proc)
           (core/extend-env (.params proc) args (.env proc))))
        :else (throw (ex-info  "Unknown procedure type: "  
                              {:proc proc :args args}))))

;; returns fn: (env) -> evaluation result
(defprotocol IAnalyze
  (analyze [exp]))

(defn analyze-if [[_ pred conseq alt]]
  (let [pred-fn (analyze pred)
        conseq-fn (analyze conseq)
        alt-fn (analyze alt)]
    (fn [env]
      (if (true? (pred-fn env))
        (conseq-fn env)
        (alt-fn env)))))

(defn analyze-sequence [sq]
  (let [sequentially (fn [f1 f2]
                       (fn [env] (f1 env) (f2 env)))
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
    (fn [env] 
      (Proc. params body-fn env _name))))

(defn analyze-quoted [[_ quotation]]
  (fn [_] quotation))

(defn analyze-assignment [[_ _name v]]
  (let [val-fn (analyze v)]
    (fn [env] 
      (core/set-variable-value! _name (val-fn env) env))))

(defn analyze-def [[_ _name v]]
  (let [val-fn (analyze v)]
    (fn [env] 
      (core/define-variable! _name (val-fn env) env))))

(defn analyze-defn [[_ _name params & body]]
  (let [make-proc-fn (analyze-lambda params body _name)]
    (fn [env]
      (core/define-variable! _name (make-proc-fn env) env))))

(defn analyze-fn [[_ params & body]]
  (let [make-proc-fn (analyze-lambda params body nil)]
    #(make-proc-fn %)))

(defn analyze-application [[op & operands]]
  (let [f-fn (analyze op)
        args-fns (map analyze operands)]
    (fn [env]
      (execute-applicaiton
        (f-fn env)
        (map #(% env) args-fns)))))

(defn analyze-let [sexp]
  (analyze (core/let->lambda sexp)))

(extend-protocol IAnalyze
  nil (analyze [_] (fn [_] nil))

  java.lang.Boolean (analyze [s] (fn [_] s))
  java.lang.Long (analyze [s] (fn [_] s))
  java.lang.Character (analyze [s] (fn [_] s))
  java.lang.String (analyze [s] (fn [_] s))
  clojure.lang.IPersistentVector (analyze [s] (fn [_] s))

  clojure.lang.Symbol
  (analyze [sexp]
    (fn [env] (core/lookup-variable-value env sexp))) 

  clojure.lang.ISeq
  (analyze [[op :as sexp]]
    (case op
      set! (analyze-assignment sexp) 
      def (analyze-def sexp)
      defn (analyze-defn sexp)
      fn (analyze-fn sexp)
      if (analyze-if sexp)
      do (analyze-sequence (next sexp))
      quote (analyze-quoted sexp) 
      let (analyze-let sexp)
      (analyze-application sexp))))

(comment 
  (def test-env (core/extend-env core/global-env))
  (core/define-variable! 'a 122 test-env)
  (core/set-variable-value! 'a 129 test-env)
  (core/lookup-variable-value test-env 'a)

  (def p (Proc. '(a b c) '((+ a b c)) test-env 'factorial))
  (core/define-variable! 'f p test-env)

  #_(-apply p '(1 2 3))
  )

;; Evaluation

(defn -eval [sexp env]
  ((analyze sexp) env))

;; Run program

(defn eval-program [sexps]
  (let [env (core/extend-env)]
    (last (map #(-eval % env) sexps))))

(comment
  (eval-program '(
                  (let [a 1, b 2]
                    (println "a")
                    (+ a b))
                  ))
  )

;; REPL

; (deftype Proc [params body env name])
(defn user-print [obj]
  (if (core/compound-procedure? obj)
    (clojure.pprint/pprint 
      {:compound-procedure (.name obj)
       :params (.params obj)})
    (println obj)))

(defn driver-loop []
  (loop [input (read)
         env (core/extend-env)]
    (if (= (str input) "exit")
      nil
      (do 
        (print ">")
        (user-print (-eval input env))
        (recur (read) env)))))
