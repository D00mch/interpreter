(ns dumch.draft.cps )

(set! *warn-on-reflection* false)

;;; # Env

(defrecord IEnv [symbols outer])

(def primitive-fns {'+ + '- - '* * '/ / '= = 'identity identity})

(def empty-env (->IEnv {} nil))
(def top-env (->IEnv (merge {'x 2, 'y 4, 'z 6 } primitive-fns) 
                     empty-env))

(def primitive-fn? (set (vals primitive-fns)))

;;; # Interpreter in Continuation Passing Style

#_(defprotocol IEvalCPS (eval-cps [this env k]))
(defprotocol IAnalyze (analyze [sexp]))
(declare eval-cps)

(defn lookup-env-k [{:keys [symbols outer] :as env} x succ fail]
  (cond 
    (= env empty-env) (fail)
    (contains? symbols x) (succ (get symbols x))
    :else (lookup-env-k outer x succ fail)))

(comment 
  (lookup-env-k top-env 'x identity #(println :fuck!)))


(def top-k (fn [r] 
             (println :error r)
             r))

(defn get-args [[arg-f & arg-fs] env k]
  (if arg-f
    (arg-f
      env
      (fn [v]
        (get-args arg-fs 
                  env 
                  (fn [vs]
                    (k (cons v vs))))))
    (k '())))

;(get-args [1 2 '(+ 1 2)] top-env identity)

(defrecord Continuation [env k])

(defn eval-seq [[h & tail] env k]
  (cond tail #(eval-cps h 
                        env 
                        (fn [_]
                          (eval-seq tail env k)))
        h #(eval-cps h env k)))

(defn analyze-self-eval [sexp]
  (fn [env k] (k sexp)))

(defn analyze-lookup [sexp]
  (fn [env k] 
    (lookup-env-k env 
                  sexp
                  k 
                  #(top-k (format "%s not bound" sexp)))))

(defn analyze-if [[_ test-expr then else]]
  (let [test-fn (analyze test-expr)
        then-fn (analyze then)
        else-fn (analyze else)]
    (fn [env k] 
      (test-fn env
               (fn [b]
                 (if (boolean? b)
                   ((if b then-fn else-fn) env k)
                   (top-k (format "%s not a boolean" b))))))))

(defn analyze-fn [sexp]
  (let [[_ args & body] sexp]
    (fn [env k] 
      (k {:params args :body body :env env}))))

(defn analyze-call-cc [sexp]
  (let [[_ f] sexp]
    (fn [env k] 
      (eval-cps (list f (Continuation. env k)) env k))))

(defn analyze-application [[op & args]]
  (let [pf (analyze op)
        args-fn (map analyze args)]
    (fn [env k]
      (pf
        env
        (fn [p]
          (get-args 
            args-fn
            env 
            (fn [as]
              (cond 
                (primitive-fn? p) (k (apply p as))
                (instance? Continuation p)
                ((.k p) (first as))
                :else (trampoline eval-seq (:body p)
                                  (->IEnv (zipmap (:params p) as) 
                                          (:env p))
                                  k)))))))))

#trace
(extend-protocol IAnalyze
  nil
  (analyze [_] (fn [_ k] (k nil)))

  java.lang.Long
  (analyze [this] 
    (analyze-self-eval this))

  java.lang.Boolean
  (analyze [this] 
    (analyze-self-eval this))

  clojure.lang.Symbol
  (analyze [this]
    (analyze-lookup this))

  Continuation
  (analyze [this]
    (fn [_ k] (k this)))

  clojure.lang.ISeq
  (analyze [[op :as sexp]]
    (case op
      ifte (analyze-if sexp)
      call-cc (analyze-call-cc sexp)
      fn (analyze-fn sexp)
      (analyze-application sexp))))

#trace
(defn eval-cps [sexp env k]
  (fn [] ((analyze sexp) env k)))

(defn run [exp]
  (trampoline eval-cps exp top-env identity))

(comment 
  (run '(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 6))))))

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

  #_(require '[clj-java-decompiler.core :refer [decompile]])

  ,)
