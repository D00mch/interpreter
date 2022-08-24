(ns dumch.interpreter)

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

(deftype Proc [params body env name])

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

;; analyze

;; returns fn: (env) -> evaluation result
(defprotocol Ianalyze
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
      (set-variable-value! _name (val-fn env) env))))

(defn analyze-def [[_ _name v]]
  (let [val-fn (analyze v)]
    (fn [env] 
      (define-variable! _name (val-fn env) env))))

(defn analyze-defn [[_ _name params & body]]
  (let [make-proc-fn (analyze-lambda params body _name)]
    (fn [env]
      (define-variable! _name (make-proc-fn env) env))))

(defn analyze-fn [[_ params & body]]
  (let [make-proc-fn (analyze-lambda params body nil)]
    #(make-proc-fn %)))

(defn- analyze-application [[op & operands]]
  (let [f-fn (analyze op)
        args-fns (map analyze operands)]
    (fn [env]
      (execute-applicaiton
        (f-fn env)
        (map #(% env) args-fns)))))

(extend-protocol Ianalyze
  nil (analyze [_] nil)

  java.lang.Boolean (analyze [s] (fn [_] s))
  java.lang.Long (analyze [s] (fn [_] s))
  java.lang.Character (analyze [s] (fn [_] s))
  java.lang.String (analyze [s] (fn [_] s))
  clojure.lang.IPersistentVector (analyze [s] (fn [_] s))

  clojure.lang.Symbol
  (analyze [sexp]
    (fn [env] (lookup-variable-value env sexp))) 

  clojure.lang.ISeq
  (analyze [[op :as sexp]]
    (case op
      set! (analyze-assignment sexp) 
      def (analyze-def sexp)
      defn (analyze-defn sexp)
      fn (analyze-fn sexp)
      if (analyze-if sexp)
      quote (analyze-quoted sexp) 
      (analyze-application sexp))))

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
  ((analyze sexp) env))

;; Run program

(defn eval-program [sexps]
  (let [env (extend-env '() '() global-env)]
    (last (map #(-eval % env) sexps))))

;; REPL

; (deftype Proc [params body env name])
(defn user-print [obj]
  (if (compound-procedure? obj)
    (clojure.pprint/pprint 
      {:compound-procedure (.name obj)
       :params (.params obj)})
    (println obj)))

(defn driver-loop []
  (loop [input (read)
         env (extend-env '() '() global-env)]
    (if (= (str input) "exit")
      nil
      (do 
        (print ">")
        (user-print (-eval input env))
        (recur (read) env)))))

(comment 

  (driver-loop)

  (eval-program '( 
                  (def a 1)
                  (set! a 2)
                  a ))

  (eval-program '( (quote (+ 1 2 3)) ))

  (eval-program '((count [1 2 3])))

  (eval-program '((defn factorial [n]
                    (if (= n 1)
                      1
                      (* n (factorial (- n 1))))) 
                  (def a (factorial 4))
                  ((fn [] (+ 3 a)))))

  (eval-program '((def a 2)
                  (defn test []
                    (def a 2)
                    (def b 3)
                    (set! a 1)
                    (* a a a))
                  (cons (= (test) 1) (cons a '()))
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

  #_(eval-program '((defn try [a b]
                      (if (= a 0) 1 b))
                    (try 0 (/ 1 0))))

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

  ; after analyze
  ; Execution time mean : 42.358229 µs

  )
