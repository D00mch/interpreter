(ns dumch.impl.cps-stackbased
  (:require
    [clojure.spec.alpha :as s]
    [clojure.core.specs.alpha :as specs]
    [clojure.string :as str]
    [dumch.environment :as core])
  (:import
    (dumch.environment Proc)))

(set! *warn-on-reflection* 1)

(defprotocol IAnalyze
  (analyze [sepx]))

(deftype Continuation [k])

(deftype Native [sym])

(defrecord Arity [params var-params body])
(defrecord Defn [arities env fn-name])

(defn analyze-self-evaluating [sexp]
  (fn [_ ds k]
    #(k (conj ds sexp))))

(defn analyze-sequence [sq]
  (let [sequentially 
        (fn [f1 f2]
          (fn [env ds k]
            (fn [] (f1
                     env
                     ds
                     (fn [ds2]
                       (f2 env ds2 k))))))
        [f & fs] (map analyze sq)]
    (if (nil? f) 
      (fn [_ ds k] #(k ds))
      (loop [f f
             fs fs]
        (if (seq fs)
          (recur (sequentially f (nth fs 0))
                 (next fs))
          f)))))

(defn analyze-if [sexp]
  (let [[[_ & conseq] [_ & alt]] (split-with (partial not= 'else>) sexp)
        conseq-fn (analyze-sequence conseq)
        alt-fn (analyze-sequence alt)]
    (fn [env ds k]
      (let [pred (peek ds)
            stack* (pop ds)
            env* (core/extend-env env)
            r (if (or (false? pred) (nil? pred))
                #(alt-fn env* stack* k)
                #(conseq-fn env* stack* k))]
        r))))

(defn analyze-when [sexp]
  (let [conseq-fn (analyze (rest sexp))]
    (fn [env ds k]
      (let [pred (peek ds)
            stack* (pop ds)
            env* (core/extend-env env)
            r (if (or (false? pred) (nil? pred))
                #(k (pop ds))
                #(conseq-fn env* stack* k))]
        r))))

(defn analyze-call [_]
  (fn [env ds k]
    (let [sq (peek ds)
          ds* (pop ds)
          env* (core/extend-env env)]
      #((analyze sq) env* ds* k))))

(defn analyze-call-cc [sexp]
  (let [sq-fn (analyze (rest sexp))]
    (fn [env ds k]
      (sq-fn env
             (conj ds (Continuation. k))
             (fn [ds*]
               #(k ds*))))))

(defn generate-call-cc [sym]
  (let [cc-function (analyze (list 'invoke> sym 0))]
    (fn [env ds k] 
      (cc-function env ds (fn [cc]
                            #(k (conj ds cc)))))))

(defn loop-vals [body-fn [v & tail] env ds k]
  (if v
    #(let [k2 (fn [ds2]
               (loop-vals body-fn tail env ds2 k))]
      (core/define-variable! '<continue_> (Continuation. k2) env)
      (body-fn env (conj ds v) k2))
    #(k ds)))

(defn loop-count [body-fn cnt env ds k]
  (if (> cnt 0)
    #(let [k2 (fn [ds2]
                (loop-count body-fn (dec cnt) env ds2 k))]
       (core/define-variable! '<continue_> (Continuation. k2) env)
       (body-fn env ds k2))
    #(k ds)))

(defn analyze-loop [sexp loop-fn]
  (let [for-fn (analyze (rest sexp))]
    (fn [env ds k]
      (core/define-variable! '<break_> (Continuation. k) env)
      #(loop-fn for-fn (peek ds) env (pop ds) k))))

(defn analyze-each [sexp]
  (analyze-loop sexp loop-vals))

(defn analyze-times [sexp]
  (analyze-loop sexp loop-count))

(defn analyze-quoted [[op :as sexp]]
  (analyze-self-evaluating (case op
                             quote> (next sexp)
                             quote (second sexp))))

(defn analyze-assignment [sexp]
  (fn [env ds k] 
    (let [value (peek ds)]
      (core/set-variable-value! sexp value env)
      #(k ds))))

(defn analyze-def [sexp]
  (let [^String _name (name sexp)
        sym (symbol (subs _name 0 (dec (.length _name))))]
    (fn [env ds k] 
      (let [value (peek ds)]
        (core/define-variable! sym value env)
        #(k ds)))))

#trace
(defn analyze-native [sexp]
  (let [native (Native. sexp)]
    (fn [_ ds k]
      #(k (conj ds native)))))

(defn analyze-lookup [sym]
  (fn [env stack k]
    (let [result (core/lookup-variable-value env sym)]
      #(k (conj stack result)))))

(defn- map-params [{{:keys [params var-params]} :params body :body}]
  (Arity. (mapv second params)
          (-> var-params :var-form second)
          (analyze-sequence (seq (second body)))))

(defn analyze-defn [[_ & f]]
  (let [{:keys [fn-name fn-tail]} (s/conform ::specs/defn-args f)
        [arity body] fn-tail
        arities (cond (= arity :arity-1) [(map-params body)]
                      (= arity :arity-n) (mapv map-params (:bodies body)))]
    (fn [env stack k]
      (core/define-variable!
        fn-name
        (Defn. arities env fn-name)
        env)
      #(k stack))))

(defn- match-defn-arity [^Defn {:keys [arities]} args]
  (letfn [(better-match? [{:keys [params var-params] :as arity} best]
            (and var-params
                 (>= (count args) (count params))
                 (> (count (:params arity))
                    (count (:params best)))))
          (exact-match? [{:keys [params var-params]}]
            (and (= (count params) (count args)) (nil? var-params)))]
    (loop [[arity & tail] arities
           best nil]
      (cond (nil? arity) best
            (exact-match? arity) arity
            (better-match? arity best) (recur tail arity)
            :else (recur tail best)))))

(defn- zip-params-args [{:keys [params var-params]} args]
  (let [pcount (count params)
        varargs (seq (drop pcount args))
        params* (cond-> params varargs (conj var-params))
        args* (cond-> (vec (take pcount args))  varargs (conj varargs))]
    (zipmap params* args*)))

#trace
(defn execute-applicaiton [proc args ds k]
  (cond (core/primitive-procedure? proc) 
        #(k (conj ds (apply proc args)))

        (instance? Continuation proc)
        #((.k ^Continuation proc) ds)

        (instance? Defn proc)
        (let [^Arity arity (match-defn-arity proc args)
              env (core/extend-env (.env proc) 
                                   (zip-params-args arity args))]
          #((.body arity) env ds k))

        (instance? Native proc)
        #(k (conj ds (eval `(~(.sym ^Native proc) ~@args))))

        :else (throw (ex-info  "Unknown procedure type: "  
                              {:proc proc :args args}))))

(defn analyze-application [[_ op args-count]] ;; (invoke> + 2)
  (let [op-fn (analyze op)]
    (fn [env ds k]
      (let [args-count (if (number? args-count) 
                         args-count
                         (core/lookup-variable-value env args-count))
            args (take-last args-count ds)
            ds* (subvec ds 0 (- (count ds) args-count))]
        (op-fn env 
               ds 
               (fn [f]
                 #(execute-applicaiton
                   (peek f)
                   args
                   ds*
                   k)))))))

(defn- def? [^String sym]
  (and (str/starts-with? sym "!") (str/ends-with? sym "+")))

(defn- native? [^clojure.lang.Symbol sym]
  (let [^String nm (name sym)]
    (or (str/starts-with? nm ".")
        (str/ends-with? nm ".")
        (.contains (str sym) "/"))))

(defn- swap [v] 
  (let [c (count v)
        i1 (dec c)
        i2 (dec i1)]
    (assoc v i2 (v i1) i1 (v i2))))

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
      (cond (= sym '<pop>) (fn [_ ds k] #(k (pop ds)))
            (= sym '<dup>) (fn [_ ds k] #(k (conj ds (last ds)))) 
            (= sym '<swap>) (fn [_ ds k] #(k (swap ds)))
            (= sym '<call>) (analyze-call sym)
            (= sym '<continue>) (generate-call-cc '<continue_>) 
            (= sym '<break>) (generate-call-cc '<break_>)
            (def? _name) (analyze-def sym)
            (native? sym) (analyze-native sym)
            :else (analyze-lookup sym)))) 

  clojure.lang.ISeq
  (analyze [[op :as sexp]]
    (case op
      set! (analyze-assignment sexp) 
      def (analyze-def sexp)
      defn> (analyze-defn sexp)
      call/cc> (analyze-call-cc sexp)
      if> (analyze-if sexp)
      each> (analyze-each sexp)
      times> (analyze-times sexp)
      when> (analyze-when sexp)
      quote> (analyze-quoted sexp) 
      quote (analyze-quoted sexp)
      invoke> (analyze-application sexp)
      (analyze-sequence sexp))))

#_(defn eval- [sexps]
  (let [env (core/extend-env)]
    ((analyze sexps) env [] identity)))

(defn eval- 
  ([cs]
   (eval- cs (core/extend-env)))
  ([cs env]
   (eval- cs env [] identity))
  ([[h & tail :as cs] env ds k]
   (println :ds ds :cs cs)
   (if (or (some? h) (some? tail))
     (trampoline (analyze h)
                 env
                 ds
                 (fn [stack2]
                   #(eval- tail env stack2 k)))
     ds)))

(comment

  (eval- '('(2 !b+ !b)
           <call>))

  (eval- '(1 1
             (invoke> = 2)
             (if> "was true" 1 2 !c+ else> "was false")))

  (eval- '(2 !a+
             (quote> 1 2 3 !b+)
             <call>))

  (eval- '(1
           (call/cc>
            !c1+
            <pop>
            2
            (invoke> !c1 0)
            3)
           "end"))

  (eval- '(1
           !a+
           (call/cc>
            !c1+
            <pop>
            2
            (call/cc> !c2+
                      <pop>
                      true
                      (if> (invoke> !c2 0) else> 0)
                      9)
            3
            (invoke> !c1 0))
           "end"))

  (eval- '((defn> plus7
             ([!a]
              !a 7 (invoke> + 2))
             ([!a !b]
              !a
              !b
              7
              (invoke> + 3))
             ([!a !b & !sq]
              !sq
              (invoke> count 1)
              3 ;; add 3 more args: !a, !b, 7 
              (invoke> + 2)
              !args-count+
              <pop>
              !a !b
              !sq <call> ;; move sequence to stack
              7
              (invoke> + !args-count)))
           1 1 1 1
           (invoke> plus7 4)))

  (eval- '((defn> each
             [!vc !quot]
             !vc
             (if>
              !vc
              (invoke> first 1)
              !quot
              <call>
              !vc
              (invoke> next 1)
              !quot
              (invoke> each 2)
              else>
              <pop>))

           '(1 2 3 4)
           (quote>
            ">>> number: "
            <swap>
            (invoke> println 2))
           (invoke> each 2)))

  (eval- '((defn> each
             [!vc !quot]
             !vc
             (if>
              !vc
              (invoke> first 1)
              !quot
              <call>
              !vc
              (invoke> next 1)
              !quot
              (invoke> each 2)
              else>
              <pop>))

           (call/cc> !break+ <pop>
                     '(1 2 3 4)
                     (quote>
                      (call/cc> !continue+ <pop>
                        !input+
                        !input
                        (invoke> even? 1)
                        (if> (invoke> !continue 0) 
                             else> "not even" (invoke> println 1) <pop>)

                        !input
                        ">>> number: "
                        <swap>
                        (invoke> println 2)
                        <pop>
                        !input
                        3
                        (invoke> < 3)
                        (if> (invoke> !break 0) else> nil <pop>)
                        ))
                    (invoke> each 2)
                    )
                  ))

  (require '[flow-storm.api :as fs-api])
  (fs-api/local-connect)
  (fs-api/stop)

  )
