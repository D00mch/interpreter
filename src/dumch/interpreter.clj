(ns dumch.interpreter
  (:gen-class))

(set! *warn-on-reflection* true)

(defn error [& msg] (assert false (apply str msg)))

;; # Environment

(defrecord State [result env])

(defn set-variable-value [-name value env]
  (if (contains? env -name)
    (State. 'NIL (assoc env -name value))
    (error "Unbound variable: " -name)))

(defn define-variable [-name value env]
  (State. 'NIL (assoc env -name value)))

;; # Procedure

(declare eval-sequence eval-to-result)

(deftype Proc [params body env name])

(def primitive-procedure-map 
  {'+ +, '- -, '* *, '/ / 
   '= (fn [& args]
        (if (apply = args) 'TRUE 'FALSE)) })

(def primitive-procedure-name? (set (keys primitive-procedure-map)))
(def primitive-procedure? (set (vals primitive-procedure-map)))

(def compound-procedure? #(-> % class (= Proc)))

(defn -apply [proc args]
  (cond (primitive-procedure? proc) (apply proc args)
        (compound-procedure? proc) 
        (let [proc ^Proc proc]
          (eval-sequence (.body proc)
                         (merge (.env proc)
                                {(.name proc) proc}
                                (zipmap (.params proc) args))))
        #_(eval>result (.body proc)
               (merge (.env proc)
                      {(.name proc) proc}
                      (zipmap (.params proc) args)))
        :else (error "Unknown procedure type: " proc args)))

;; Boolean

(def bools #{'TRUE 'FALSE})

(defn self-evaluating? [sexp]
  ((some-fn number? bools string? char?) sexp))

(defn _true? [sexp]
  (and (not= 'FALSE sexp) (not= 'NIL sexp)))

;; Evaluation

(defprotocol IEval
  (-eval [this env]))

(defn eval-to-result [sexp env]
  (:result (-eval sexp env)))

(defn eval-if [[_ pred conseq alt] env]
  (cond (_true? (eval-to-result pred env)) (eval-to-result conseq env)
        (nil? alt) 'NIL
        :else (eval-to-result alt env)))

(defn eval-sequence [[head & tail] env]
  ; (println :head head :tail tail)
  (cond tail (do (-eval head env)
                 (recur tail env))
        head (eval-to-result head env)))

(extend-protocol IEval
  clojure.lang.Symbol
  (-eval [sexp env]
    (cond
      (primitive-procedure-name? sexp) 
      (State. (primitive-procedure-map sexp) env)
      :else (State. (env sexp) env))) 

  clojure.lang.ISeq
  (-eval [sexp env]
    (let [[op & operands] sexp]
      (case op
        set! (let [[_name exp] operands
                   value (eval-to-result exp env)]
               (set-variable-value _name value env))
        def (let [[_name exp] operands
                  value (eval-to-result exp env)]
              (define-variable _name value env))

        defn (let [[_name params & body] operands
                   new-fn (Proc. params body env _name)]
               (define-variable _name new-fn env))

        if (State. (eval-if sexp env) env)

        fn (let [[params & body] operands]
             (State. (Proc. params body env nil) env))

        (State. (-apply (eval-to-result op env)
                        (map #(eval-to-result % env) operands))
                env))))

  Object
  (-eval [sexp env]
    (cond 
      (self-evaluating? sexp) (State. sexp env)
      :else (error "EVAL FAIL: " sexp))))

;; Run program

(defn next-state [last-state sexp]
  (let [env (:env last-state)]
    (-eval sexp env)))

(def initial-state (State. 'NIL {}))

(defn eval-program [sexps]
  (reduce next-state initial-state sexps))

(comment 

  (eval-program '((defn factorial [n]
                    (if (= n 1)
                      1
                      (* n (factorial (- n 1))))) 
                  (def a (factorial 4))
                  ((fn [] (+ 3 a)))))
  
  (require '[portal.api :as p])

  (def p (p/open))
  (add-tap #'p/submit)
  
  )
