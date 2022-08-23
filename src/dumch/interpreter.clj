(ns dumch.interpreter
  (:gen-class))

(set! *warn-on-reflection* true)

;; # Environment

(deftype Frame [^clojure.lang.IPersistentMap bindings outer])

(def primitive-procedure-map 
  {'+ +, '- -, '* *, '/ / 'rem rem 'quot quot 'nil? nil?  
   'first first 'rest rest 'last last 'cons cons 'count count 
   '= (fn [& args]
        (if (apply = args) 'true 'false)) })

(def global-env (Frame. primitive-procedure-map nil))

(defn lookup-variable-value [^Frame env x]
  (cond 
    (nil? env) (throw (ex-info "not found" {:x x :env env}))
    (contains? (.bindings env) x) (get (.bindings env) x)
    :else (recur (.outer env) x)))

(defn extend-env [variables values base-env]
  (Frame. (java.util.HashMap. 
            ^java.util.Map (zipmap variables values)) base-env))

(defn define-variable! [-name value ^Frame env]
  (.put ^java.util.HashMap (.bindings env) -name value)
  nil)

(defn set-variable-value! [-name value ^Frame env]
  (if (lookup-variable-value env -name)
    (define-variable! -name value env)
    (throw (ex-info "Unbound variable: " {:name -name}))))

;; # Procedure

(defrecord Proc [params body env name])

(def primitive-procedure? (set (vals primitive-procedure-map)))
(def compound-procedure? #(-> % class (= Proc)))

(defn execute-applicaiton [proc args]
  (cond (primitive-procedure? proc) (apply proc args)
        (compound-procedure? proc)
        (let [proc ^Proc proc] 
          ((.body proc)
           (extend-env (.params proc) args (.env proc))))
        :else (throw (ex-info  "Unknown procedure type: "  
                              {:proc proc :args args}))))

;; Analize

;; returns fn: (env) -> evaluation result
(defprotocol IAnalize
  (analize [exp]))

(defn analize-if [sexp]
  (let [[_ pred conseq alt] sexp
        pred-fn (analize pred)
        conseq-fn (analize conseq)
        alt-fn (analize alt)]
    (fn [env]
      (if (true? (pred-fn env))
        (conseq-fn env)
        (alt-fn env)))))

(defn analize-sequence [sq]
  (loop [sq sq
         r []]
    (if (seq sq)
      (recur (next sq)
             (conj r (analize (nth sq 0))))
      (fn [env] 
        (last (map #(% env) r))))))

(defn analize-lambda [params body-sq]
  (let [body-fn (analize-sequence body-sq)]
    (fn [env] 
      (Proc. params body-fn env nil))))

(defn analize-quoted [[_ quotation]]
  (fn [_] quotation))

(defn analize-assignment [[_ _name v]]
  (fn [env] 
    (set-variable-value! _name ((analize v) env) env)))

(defn analize-def [[_ _name v]]
  (fn [env] 
    (define-variable! _name ((analize v) env) env)))

(defn analize-defn [[_ _name params & body]]
  (let [make-proc-fn (analize-lambda params body)]
    (fn [env]
      (define-variable! _name (make-proc-fn env) env))))

(defn analize-fn [[_ params & body]]
  (let [make-proc-fn (analize-lambda params body)]
    #(make-proc-fn %)))

(defn- analize-application [[op & operands]]
  (let [f-fn (analize op)
        args-fns (map analize operands)]
    (fn [env]
      (execute-applicaiton
        (f-fn env)
        (map #(% env) args-fns)))))

(extend-protocol IAnalize
  nil (analize [_] nil)

  java.lang.Boolean (analize [s] (fn [_] s))
  java.lang.Long (analize [s] (fn [_] s))
  java.lang.Character (analize [s] (fn [_] s))
  java.lang.String (analize [s] (fn [_] s))
  clojure.lang.IPersistentVector (analize [s] (fn [_] s))

  clojure.lang.Symbol
  (analize [sexp]
    (fn [env] (lookup-variable-value env sexp))) 

  clojure.lang.ISeq
  (analize [[op :as sexp]]
    (case op
      set! (analize-assignment sexp) 
      def (analize-def sexp)
      defn (analize-defn sexp)
      fn (analize-fn sexp)
      if (analize-if sexp)
      quote (analize-quoted sexp) 
      (analize-application sexp))))

(comment 
  (def test-env (extend-env '() '() global-env))
  (define-variable! 'a 122 test-env)
  (set-variable-value! 'a 129 test-env)
  (lookup-variable-value test-env 'a)

  (def p (->Proc '(a b c) '((+ a b c)) test-env 'factorial))
  (define-variable! 'f p test-env)

  #_(-apply p '(1 2 3))
  )

;; Evaluation

(defn -eval [sexp env]
  ((analize sexp) env))

;; Run program

(defn eval-program [sexps]
  (let [env (extend-env '() '() global-env)]
    (last (map #(-eval % env) sexps))))

(comment 
  (eval-program '( 
                  (def a 1)
                  (set! a 2)
                  a ))

  (eval-program '( (cons 1 '()) ))

  (eval-program '((count [1 2 3])))

  (eval-program '((defn factorial [n]
                    (if (= n 1)
                      1
                      (* n (factorial (- n 1))))) 
                  (def a (factorial 4))
                  ((fn [] (+ 3 a)))))

  (eval-program '((def a 2)
                  (defn test []
                    (def a 1)
                    (def b 3)
                    (* a a a))
                  (cons (= (test) 1) '())
                  ))

  (eval-program '(
                  (defn f [x]
                    (defn even? [n]
                      (if (= n 0)
                        true
                        (odd? (- n 1))))
                    (defn odd? [n]
                      (if (= n 0)
                        false
                        (even? (- n 1))))
                    (even? x))
                  (f 4)
                  ))
  
  (require '[portal.api :as p])
  (def p (p/open))
  (add-tap #'p/submit)

  (require '[criterium.core :refer [quick-bench]])

  (quick-bench
    (eval-program '((defn factorial [n]
                      (if (= n 1) 1 (* n (factorial (- n 1))))) 
                    (factorial 20))))

  ; object 
  ; Execution time mean : 51.355074 µs

  ; dispatch on concrete types
  ; Execution time mean : 49.512234 µs

  ; after analize
  ; Execution time mean : 47.358229 µs

  )
