(ns dumch.draft.cps)

;;; # Env

(defrecord IEnv [symbols outer])

(def primitive-fns {'+ + '- - '* * '/ / '= = 'identity identity})

(def empty-env (->IEnv {} nil))
(def top-env (->IEnv (merge {'x 2, 'y 4, 'z 6 } primitive-fns) 
                     empty-env))

(def primitive-fn? (set (vals primitive-fns)))

;;; # Interpreter in Continuation Passing Style

(defprotocol IEvalCPS (eval-cps [this env k]))

(defn lookup-env-k [{:keys [symbols outer] :as env} x succ fail]
  (cond 
    (= env empty-env) (fail)
    (contains? symbols x) (succ (get symbols x))
    :else (lookup-env-k outer x succ fail)))

(def top-k (fn [r] 
             (println :error r)
             r))

(defn get-args [[arg & args] env k]
  (if arg
    (eval-cps arg
              env
              (fn [v]
                (get-args args 
                          env 
                          (fn [vs]
                            (k (cons v vs))))))
    (k '())))

;(get-args [1 2 '(+ 1 2)] top-env identity)

(defrecord Continuation [env k])

(defn eval-seq [[h & tail] env k]
  (cond tail (eval-cps h 
                       env 
                       (fn [_]
                         (eval-seq tail env k)))
        h (eval-cps h env k)))

(extend-protocol IEvalCPS
  java.lang.Long
  (eval-cps [this _ k] (k this))

  java.lang.Boolean
  (eval-cps [this _ k] (k this))

  clojure.lang.Symbol
  (eval-cps [this env k]
    (lookup-env-k env 
                  this
                  k 
                  #(top-k (format "%s not bound" this))))

  Continuation
  (eval-cps [this _ k] (k this))

  clojure.lang.ISeq
  (eval-cps [[op :as sexp] env k]
    (case op
      ifte 
      (let [[_ test-expr then else] sexp] 
        (eval-cps test-expr
                  env
                  (fn [b]
                    (if (boolean? b)
                      (eval-cps (if b then else) env k)
                      (top-k (format "%s not a boolean" b))))))

      call-cc (let [[_ f] sexp]
                (eval-cps (list f (Continuation. env k)) env k))

      fn (let [[_ args & body] sexp]
           (k {:params args :body body :env env}))

      (let [[op & args] sexp] 
        (eval-cps 
          op 
          env
          (fn [p]
            (println :p p)
            (get-args 
              args 
              env 
              (fn [as]
                (cond 
                  (primitive-fn? p) (k (apply p as))
                  (instance? Continuation p)
                  ((.k p) (first as))
                  :else (eval-seq (:body p)
                                  (->IEnv (zipmap (:params p) as) 
                                          (:env p))
                                  k))))))))))

(defn run [exp]
  (eval-cps exp top-env identity))

(comment 
  (lookup-env-k top-env 'x identity #(println :fuck!))

  (run '(call-cc (fn [k0]
                   (+ 1
                      2 
                      (k0 1)))))

  (run '(call-cc (fn [k] (k 0) 1))) ;;=> 0
  (run '(call-cc (fn [k] (ifte true (k 0) 1)))) ;;=> 0

  (run '(ifte (= x 2) 1 0))
  (run '(= x 2))

  (run '((fn [a] (+ a a)) 1))

  (run '(+ (ifte (= x 2) 1 0) (+ 1 1) 3))

  (run '(ifte true (ifte true z y) y))

  (require '[flow-storm.api :as fs-api])
  (fs-api/local-connect)
  (fs-api/stop)
  ,)
