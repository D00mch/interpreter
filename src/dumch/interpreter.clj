(ns dumch.interpreter
  (:gen-class))

(set! *warn-on-reflection* true)

(defn error [& msg] (assert false (apply str msg)))

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
            ^java.util.Map (zipmap variables values)) base-env)
  #_(Frame. (zipmap variables values) base-env)
  #_(merge base-env
         #_{(.name proc) proc}
         (zipmap variables values)))

(defn define-variable! [-name value ^Frame env]
  (.put ^java.util.HashMap (.bindings env) -name value)
  nil
  #_(State. nil (assoc-in env [:bindings -name] value)))

(defn set-variable-value! [-name value ^Frame env]
  (if (lookup-variable-value env -name)
    (define-variable! -name value env)
    (error "Unbound variable: " -name)))

;; # Procedure

(declare eval-sequence eval-to-result)

(defrecord Proc [params body env name])

(def primitive-procedure-name? (set (keys primitive-procedure-map)))
(def primitive-procedure? (set (vals primitive-procedure-map)))

(def compound-procedure? #(-> % class (= Proc)))

(defn -apply [proc args]
  (cond (primitive-procedure? proc) (apply proc args)
        (compound-procedure? proc) 
        (let [proc ^Proc proc]
          (eval-sequence (.body proc)
                         (extend-env (.params proc) 
                                     args  
                                     (.env proc))))
        :else (error "Unknown procedure type: " proc args)))

(comment 
  (def test-env (extend-env '() '() global-env))
  (define-variable! 'a 122 test-env)
  (set-variable-value! 'a 129 test-env)
  (lookup-variable-value test-env 'a)

  (def p (->Proc '(a b c) '((+ a b c)) test-env 'factorial))
  (define-variable! 'f p test-env)

  (-apply p '(1 2 3))
  )


;; Assignment

(defrecord Assignment [variable value])
(defrecord Definition [variable value])

;; Evaluation

(defprotocol IEval
  (-eval [this env]))

(defn eval-assignment [^Assignment exp env]
  (set-variable-value! (.variable exp)
                      (-eval (.value exp) env)
                      env))

(defn eval-definition [^Definition exp env]
  (define-variable! (.variable exp)
                   (-eval (.value exp) env)
                   env))

(defn eval-to-result [sexp env]
  (-eval sexp env)
  #_(.result ^State (-eval sexp env)))

(defn eval-if [[_ pred conseq alt] env]
  (cond (true? (eval-to-result pred env)) (eval-to-result conseq env)
        (nil? alt) nil
        :else (eval-to-result alt env)))

(defn eval-sequence [[head & tail] env]
  ; (clojure.pprint/pprint {:head head :tail tail :env env})
  (cond tail (do (-eval head env)
                 (recur tail ^Frame env))
        head (eval-to-result head env)))

(extend-protocol IEval
  nil (-eval [_ _] nil)

  java.lang.Boolean (-eval [s _] s)
  java.lang.Long (-eval [s _] s)
  java.lang.Character (-eval [s _] s)
  java.lang.String (-eval [s _] s)
  clojure.lang.IPersistentVector (-eval [s _] s)

  clojure.lang.Symbol
  (-eval [sexp env]
    (cond
      (primitive-procedure-name? sexp) (primitive-procedure-map sexp)
      :else (lookup-variable-value env sexp))) 

  clojure.lang.ISeq
  (-eval [sexp env]
    (let [[op & operands] sexp]
      (case op
        set! (let [[_name exp] operands
                   value (eval-to-result exp env)]
               (set-variable-value! _name value env))

        def (let [[_name exp] operands
                  value (eval-to-result exp env)]
              (define-variable! _name value env))

        defn (let [[_name params & body] operands
                   new-fn (Proc. params body env _name)]
               (define-variable! _name new-fn env)
               nil)

        if (eval-if sexp env)

        fn (let [[params & body] operands]
             (Proc. params body env nil))

        (-apply (eval-to-result op env)
                (map #(eval-to-result % env) operands))))))

;; Run program

(defn eval-program [sexps]
  (let [env (extend-env '() '() global-env)]
    (last (map #(-eval % env) sexps))))

(comment 
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
                  (cons (= (test) 1) nil)
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
                  (f 3)
                  ))
  
  (require '[portal.api :as p])
  (def p (p/open))
  (add-tap #'p/submit)

  (require '[criterium.core :refer [quick-bench]])
  )
