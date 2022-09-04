(ns dumch.impl.cps
  (:require [clojure.pprint :refer [pprint]]
            [dumch.environment :as core])
  (:import (dumch.environment Proc Frame)))

(set! *warn-on-reflection* true)

(defprotocol IAnalyze
  (analyze [exp]))

(deftype Continuation [env k])

(defn eval-cps [sexp env k]
  (trampoline (analyze sexp) env k))

(defn analyze-quoted [[_ quotation]]
  (fn [_ k] #(k quotation)))

(defn analyze-self-evaluating [exp]
  (fn [_ k] #(k exp)))

(defn analyze-assignment [[_ _name v]]
  (let [val-fn (analyze v)]
    (fn [env k] 
      (val-fn 
        env 
        (fn [v]
          #(k (core/set-variable-value! _name v env)))))))

(defn analyze-if [[_ pred conseq alt]]
  (let [pred-fn (analyze pred)
        conseq-fn (analyze conseq)
        alt-fn (analyze alt)]
    (fn [env k]
      (pred-fn env (fn [b]
                     (if b 
                       #(conseq-fn env k)
                       #(alt-fn env k)))))))

(defn analyze-sequence [sq]
  (let [sequentially (fn [f1 f2]
                       (fn [env k]
                         (trampoline f1
                                     env
                                     (fn [_]
                                       #(f2 env k)))))
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
    (fn [env k]
      (k (Proc. params body-fn env _name)))))

(defn analyze-defn [[_ _name params & body]]
  (let [val-fn (analyze-lambda params body _name)]
    (fn [env k] 
      (val-fn env (fn [v]
                    #(k (core/define-variable! _name v env)))))))

(defn analyze-def [[_ _name v]]
  (let [val-fn (analyze v)]
    (fn [env k] 
      (val-fn env (fn [v]
                    #(k (core/define-variable! _name v env)))))))


(defn analyze-fn [[_ params & body]]
  (let [make-proc-fn (analyze-lambda params body nil)]
    (fn [env k] 
      #(make-proc-fn env k))))

(defn analyze-let [sexp]
  (analyze (core/let->lambda sexp)))

(defn execute-applicaiton [proc args k]
  (cond (instance? Continuation proc)
        ((.k ^Continuation proc) (first args))
       
        (core/primitive-procedure? proc) (k (apply proc args))

        (core/compound-procedure? proc)
        (let [proc ^Proc proc] 
          ((.body proc)
           (core/extend-env (.params proc) args (.env proc))
           k))
        :else (throw (ex-info  "Unknown procedure type"  
                              {:proc proc :args args}))))

(defn get-args [[arg-f & arg-fns] env k]
  (if arg-f
    (arg-f env (fn [v]
                 #(get-args arg-fns 
                           env 
                           (fn [vs]
                             (k (cons v vs))))))
    (k '())))

(defn analyze-application [[op & operands]]
  (let [f-fn (analyze op)
        args-fns (map analyze operands)]
    (fn [env k]
      (f-fn env (fn [f]
                  (get-args args-fns 
                            env 
                            (fn [args]
                              #(execute-applicaiton
                                f
                                args
                                k))))))))

(defn analyze-call-cc [[_ f]]
  (fn [e k]
    ((analyze (list f (Continuation. e k)))
     e
     k)))

(extend-protocol IAnalyze
  nil (analyze [s] (analyze-self-evaluating s))

  java.lang.Boolean (analyze [s] (analyze-self-evaluating s))
  java.lang.Long (analyze [s] (analyze-self-evaluating s))
  java.lang.Character (analyze [s] (analyze-self-evaluating s))
  java.lang.String (analyze [s] (analyze-self-evaluating s))
  clojure.lang.IPersistentVector (analyze [s] (analyze-self-evaluating s))
  Continuation (analyze [s] (analyze-self-evaluating s))


  clojure.lang.Symbol
  (analyze [sexp]
    (fn [env k]
      (k (core/lookup-variable-value env sexp)))) 


  clojure.lang.ISeq
  (analyze [[op :as sexp]]
    (case op
      set! (analyze-assignment sexp) 
      def (analyze-def sexp)
      defn (analyze-defn sexp)
      do (analyze-sequence (next sexp))
      call-cc (analyze-call-cc sexp)
      fn (analyze-fn sexp)
      if (analyze-if sexp)
      quote (analyze-quoted sexp) 
      let (analyze-let sexp)
      (analyze-application sexp))))

(defn eval-program [sexps]
  (let [env (core/extend-env)]
    ((analyze-sequence sexps) env identity)
    #_(last (map #(eval-cps % env identity) 
               sexps))))

(comment

  (eval-program '(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 6))))))

  (eval-program '(
                  (defn recurtest [n]
                    (if (= n 0)
                      n
                      (recurtest (- n 1)))) 
                  ;; not stackoverflow
                  (recurtest 100000)))

  (eval-program '(
                  (+ 1 (if (= 2 2) 2 0))
                  ))

  (eval-program '(
                  (def a 1)
                  (set! a 2)
                  ((fn [x y] (+ x y)) a 3)
                  ))

  (eval-program '((def a 2)
                  (defn test []
                    (def a 2)
                    (def b 3)
                    (set! a 1)
                    (* a a a))
                  (cons (= (test) 1)
                        (cons a '()))))

  (eval-program '(
                  (call-cc
                    (fn [k]
                      (if true (call-cc
                                 (fn [k1]
                                   (k 0)
                                   (k1 3))))
                      1))
                  ))

  (eval-program
    '(
      (call-cc (fn [k0]
                 (+ 1
                    (call-cc (fn [k1]
                               (+ 1 (k1 3)))) 
                    (k0 1))))
      ))

  ;; TODO: fix call/cc with example below; prob. problem with let

  (eval-program '(
                  (def x 0)
                  (let [cc (call-cc (fn [k] k))]
                    (set! x (+ x 1))
                    (if (< x 10)
                      (cc cc)
                      x))))

  (require '[flow-storm.api :as fs-api])
  (fs-api/local-connect)
  (fs-api/stop)
  )
