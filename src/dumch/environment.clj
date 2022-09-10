(ns dumch.environment
  (:require [clojure.pprint :refer [pprint]]))

(set! *warn-on-reflection* true)

;; # Environment

(deftype Frame [^java.util.Map bindings outer])

(def primitive-procedure-map 
  {'nil? nil? 'string? string? 'identity identity 'some? some?  
   'boolean? boolean? 'not not 'instance? instance? 'char? char?
   'str str 'print 'print 'prn prn 'pr pr 'println println
   '= = 'not= not=

   ;; numbers
   '== == '+ +, '- -, '* *, '/ / '> > '< < '>= >=
   '<= <= '+' +', '-' '-', '*' *', 'rem rem 'quot quot
   'even? even? 'odd? odd? 'dec dec 'inc inc 'number? number?

   ;; sequence
   'first first 'last last 'rest rest 'next next 'cons cons
   'count count 'seq seq 'pop pop 'conj conj 'peek peek
   'range range 'drop drop 'take take 'empty? empty?
   'vector vector 'vec vec 'list list})

(def global-env (Frame. primitive-procedure-map nil))

(defn lookup-variable-value [^Frame env x]
  (cond 
    (nil? env) (throw (ex-info "not found" {:x x :env env}))
    (contains? (.bindings env) x) (get (.bindings env) x)
    :else (recur (.outer env) x)))

(defn extend-env 
  ([]
   (extend-env '() '() global-env))
  ([base-env]
   (extend-env '() '() base-env))
  ([base-env m]
   (Frame. (java.util.HashMap. ^java.util.Map m) base-env))
  ([variables values base-env]
   (Frame. (java.util.HashMap. 
             ^java.util.Map (zipmap variables values)) base-env)))

(defn define-variable! [-name value ^Frame env]
  (.put ^java.util.HashMap (.bindings env) -name value)
  nil)

(defn set-variable-value! [-name value ^Frame env]
  (if (lookup-variable-value env -name)
    (define-variable! -name value env)
    (throw (ex-info "Unbound variable" {:name -name}))))

;; # Procedure

(deftype Proc [params body env name]
  Object
  (toString [_]
    (with-out-str 
      (pprint {:params params, :body body, :name name}))))

; (println (Proc. [1 2] '(+ 1 2) nil nil))

(def primitive-procedure? (set (vals primitive-procedure-map)))
(def compound-procedure? #(-> % class (= Proc)))

;; # Derived

(defn let->lambda [[_ bindings & body]]
  (let [m (apply hash-map bindings) ;; todo: fix performance
        names (keys m)
        values (vals m)]
    (cons (list 'fn (vec names) 
                (cons 'do body))
          values)))
