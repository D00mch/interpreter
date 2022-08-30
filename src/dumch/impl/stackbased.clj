(ns dumch.impl.stackbased
  (:require
   [clojure.string :as str]
   [dumch.environment :as core])
  (:import
   (dumch.environment Proc)))

;; returns fn: (env, stack) -> stack'
(defprotocol IAnalyze (analyze [sexp]))

(defn analyze-self-evaluating [sexp]
  (fn [_ stack]
    (conj stack sexp)))

(defn analyze-sequence [sq]
  (let [sequentially (fn [f1 f2]
                       (fn [env stack]
                         (let [stack2 (f1 env stack)]
                           (f2 env stack2))))
        [f & fs] (map analyze sq)]
    (when (nil? f)
      (throw (ex-info "Empty sequence: analyze" {})))
    (loop [f f
           fs fs]
      (if (seq fs)
        (recur (sequentially f (nth fs 0))
               (next fs))
        f))))

(defn eval-program [sexps]
  (let [env (core/extend-env)]
    ((analyze-sequence sexps) env [])))

(defn analyze-if [sexp]
  (let [[[_ & conseq] [_ & alt]] (split-with (partial not= 'else>) sexp)
        conseq-fn (analyze-sequence conseq)
        alt-fn (analyze-sequence alt)]
    (fn [env stack]
      (let [pred (peek stack)
            _ (pr :pred pred)
            stack* (pop stack)
            env* (core/extend-env env)
            r (if (true? pred)
                (conseq-fn env* stack*)
                (alt-fn env* stack*))]
          r))))

(defn analyze-call [_]
  (fn [env stack]
    (let [sq (peek stack)
          stack* (pop stack)
          env* (core/extend-env env)]
      (assert (seq? sq) "call/cc should have quote> as an arg")
      ((analyze-sequence sq)
       env*
       stack*))))

(defn analyze-quoted [quotation]
  (analyze-self-evaluating quotation))

(defn analyze-assignment [sexp]
  (fn [env stack] 
    (let [value (peek stack)]
      (core/set-variable-value! sexp value env)
      stack)))

(defn analyze-def [sexp]
  (let [^String _name (name sexp)
        sym (symbol (subs _name 0 (dec (.length _name))))]
    (fn [env stack] 
    (let [value (peek stack)]
      (core/define-variable! sym value env)
      stack))))

(defn analyze-lookup [sym]
  (fn [env stack]
    (let [result (core/lookup-variable-value env sym)]
      (conj stack result))))

(defn execute-applicaiton [proc args stack]
  (cond (core/primitive-procedure? proc) (apply proc args)
        (core/compound-procedure? proc)
        (let [proc ^Proc proc] 
          ((.body proc)
           (core/extend-env (.params proc) args (.env proc))
           stack))
        :else (throw (ex-info  "Unknown procedure type: "  
                              {:proc proc :args args}))))

(defn analyze-application [[_ op args-count]] ;; (invoke> + 2)
  (let [op-fn (analyze op)]
    (fn [env stack]
      (let [stack* (subvec stack 0 (- (count stack) args-count))
            args (take-last args-count stack)
            result (execute-applicaiton 
                     (-> (op-fn env stack) peek) 
                     args
                     stack)]
        (conj stack* result)))))

(defn- def? [^String sym]
  (and (str/starts-with? sym "!") (str/ends-with? sym "+")))

(extend-protocol IAnalyze
  nil (analyze [s] (analyze-self-evaluating s))
  java.lang.Boolean (analyze [s] (analyze-self-evaluating s))
  java.lang.Long (analyze [s] (analyze-self-evaluating s))
  java.lang.Character (analyze [s] (analyze-self-evaluating s))
  java.lang.String (analyze [s] (analyze-self-evaluating s))
  clojure.lang.IPersistentVector (analyze [s] (analyze-self-evaluating s))

  clojure.lang.Symbol
  (analyze [sym]
    (let [^String _name (name sym)]
      (cond (= sym '<pop>) (fn [_ stack] (pop stack))
            (= sym '<call>) (analyze-call sym)
            ;; to allow call/cc we have to be able to modify call stack
            ; (= sym '<call/cc>) (analyze-call-cc sym)
            ; (= sym '<continue>) (analyze-continue sym)
            (def? _name) (analyze-def sym)
            :else (analyze-lookup sym)))) 

  clojure.lang.ISeq
  (analyze [[op :as sexp]]
    (case op
      set! (analyze-assignment sexp) 
      def (analyze-def sexp)
      ; defn (analyze-defn sexp)
      ; fn (analyze-fn sexp)
      if> (analyze-if sexp)
      do (analyze-sequence (next sexp))
      quote> (analyze-quoted (next sexp)) 
      quote (analyze-quoted (second sexp))
      ; let (analyze-let sexp)
      (analyze-application sexp))))

(comment
  (eval-program '(
                  ;; todo: idea on functions
                  (define> plus7
                    (arity> [ int? -- int? ]
                            7
                            (invoke> + 2)) 
                    (arity> [ int? int? -- int? ]
                            7
                            (invoke> + 3))
                    (arity> * 
                            [ (s/coll-of int?) -- int? ]
                            7
                            (invoke> + *)))

                  '(7 (invoke> + 2))
                  !plus7+


                  (invoke> !plus7 5)
                  (invoke> !plus7+ 5)

                  (define> plus1 1 
                    [ int? -- int? ]
                    (invoke> + 2))
                  ))

  (eval-program '(
                  1 1
                  (invoke> = 2)
                  (if> "was true" 1 2 else> "was false")
                  ))

  (eval-program '(
                  1
                  (quote>
                    1
                    (invoke> + 2))
                  <call>
                  "the end"
                  ))
  )
