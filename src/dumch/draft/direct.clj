(ns dumch.draft.direct)

;;; # Env

(defrecord IEnv [symbols outer])

(def primitive-fns {'+ + '- - '* * '/ / '= =})

(def empty-env (->IEnv {} nil))
(def top-env (->IEnv (merge {'x 2, 'y 4, 'z 6 } primitive-fns) 
                     empty-env))

(def primitive-fn? (set (vals primitive-fns)))

;;; # Direct Interpreter

(defprotocol IEval (-eval [this env]))

(defn run [exp]
  (-eval exp top-env))

(defn lookup-env [{:keys [symbols outer] :as env} x]
  (cond 
    (= env empty-env) (throw (ex-info "not found" {}))
    (contains? symbols x) (get symbols x)
    :else (lookup-env outer x)))

(extend-protocol IEval
  java.lang.Long
  (-eval [this _] this)

  java.lang.Boolean
  (-eval [this _] this)

  clojure.lang.Symbol
  (-eval [this env]
    (lookup-env env this))

  clojure.lang.ISeq
  (-eval [[op :as sexp] env]
    (case op
      ifte (let [[_ test-expr then else] sexp
                 b (-eval test-expr env)] 
             (if (boolean? b)
               (-eval (if b then else) env)
               (throw (ex-info (str "not a boolean: " b) {}))))

      fn (let [[_ args body] sexp]
           {:params args :body body :env env})

      (let [[op & args] sexp
            p (-eval op env)]
        (if (primitive-fn? p)
          (apply p (map #(-eval % env) args))
          (-eval (:body p)
                 (->IEnv (zipmap (:params p) 
                                 (map #(-eval % env) args)) 
                         (:env p))))))))

(comment
  (lookup-env top-env 'x)

  (run '((fn [a] (+ a a)) 1))

  (run '(ifte true (ifte true z y) y))


  (run '(ifte (= x 2) 1 0))
  (run '(= x 2))

  (run '((fn [a] (+ a a)) 1))

  (run '(+ (ifte (= x 2) 1 0) (+ 1 1) 3))

  (require '[flow-storm.api :as fs-api])
  (fs-api/local-connect)
  (fs-api/stop)
  ,)
