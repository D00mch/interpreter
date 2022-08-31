(ns dumch.impl.stackbased-call-cc
  (:require
    [clojure.spec.alpha :as s]
    [clojure.core.specs.alpha :as specs]
    [clojure.string :as str]
    [dumch.environment :as core])
  (:import
   (dumch.environment Proc)))

;; data stack, call stack, environment
(defrecord State [ds cs env]
  Object
  (toString [_]
    (with-out-str (clojure.pprint/pprint {:ds ds :cs cs}))))

(deftype Continuation [cs]
  Object
  (toString [_]
    (with-out-str (clojure.pprint/pprint cs))))

(defrecord Arity [params var-params body])
(defrecord Defn [arities env fn-name])

;; returns fn: State -> State
(defprotocol IAnalyze (analyze [sexp]))

(defn -eval [sexp state]
  ((analyze sexp) state))

(def inf-prot (atom 0))

(defn eval-program [{[h & tail] :cs :as state}]
  (println :ds (:ds state) :cs (:cs state))
  (if (or (some? h) (some? tail))
    ;; todo: use recur (doesn't work with #trace)
    (eval-program (-eval h (assoc state :cs tail)))
    state))

(defn- program->stack [programm]
  (reset! inf-prot 0)
  (:ds (eval-program 
         (assoc (->State [] [] (core/extend-env)) :cs programm))))

;; # Analyze

(defn analyze-self-evaluating [sexp]
  (fn [state]
    (update state :ds conj sexp)))

(defn analyze-sequence [sq]
  (let [sequentially (fn [f1 f2]
                       (fn [state]
                         (let [state2 (f1 state)]
                           (f2 state2))))
        [f & fs] (map analyze sq)]
    (when (nil? f)
      (throw (ex-info "Empty sequence: analyze" {})))
    (loop [f f
           fs fs]
      (if (seq fs)
        (recur (sequentially f (nth fs 0))
               (next fs))
        f))))

(defn analyze-if [sexp]
  (let [[[_ & conseq] [_ & alt]] 
        (split-with (partial not= 'else>) sexp)
        conseq-fn (analyze conseq)
        alt-fn (analyze alt)]
    (fn [{:keys [ds env] :as state}]
      (let [pred (peek ds)
            state* (-> (update state :ds pop)
                       (update :env core/extend-env))
            r (if (or (false? pred) (nil? pred))
                (alt-fn state)
                (conseq-fn state*))]
        (assoc r :env env)))))

(defn analyze-call [_]
  (fn [{:keys [ds env] :as state}]
    (let [sq (peek ds)
          state* (-> (update state :ds pop)
                     (update :env core/extend-env))
          q-result ((analyze sq) state*)]
      (assoc q-result :env env))))

;; call/cc ( [ quot & ds ] cs – [ cs ds ] quot )
(defn analyze-call-cc [[_ quote]]
  (let [sq-fn (analyze quote)]
    (fn [{:keys [ds cs env] :as state}]
      (let [state* (update state :ds conj (Continuation. cs))
            quote-fn-r (sq-fn state*)
            sq (peek (:ds quote-fn-r))]
        (assoc state* :cs sq)))))

(defn analyze-quoted [[op :as sexp]]
  (analyze-self-evaluating (case op
                             quote> (next sexp)
                             quote (second sexp))))

(defn analyze-assignment [sexp]
  (fn [{:keys [env ds] :as state}] 
    (let [value (peek ds)]
      (core/set-variable-value! sexp value env)
      state)))

(defn analyze-def [sexp]
  (let [^String _name (name sexp)
        sym (symbol (subs _name 0 (dec (.length _name))))]
    (fn [{:keys [env ds] :as state}] 
      (let [value (peek ds)]
        (core/define-variable! sym value env)
        state))))

(defn analyze-lookup [sym]
  (fn [{:keys [env] :as state}]
    (let [result (core/lookup-variable-value env sym)]
      (update state :ds conj result))))

(defn- map-params [{{:keys [params var-params]} :params body :body}]
  (Arity. (mapv second params)
          (-> var-params :var-form second)
          (analyze (seq (second body)))))

(defn analyze-defn [[_ & f]]
  (let [{:keys [fn-name fn-tail]} (s/conform ::specs/defn-args f)
        [arity body] fn-tail
        arities (cond (= arity :arity-1) [(map-params body)]
                      (= arity :arity-n) (mapv map-params (:bodies body)))]
    (fn [{:keys [env] :as state}]
      (core/define-variable!
        fn-name
        (Defn. arities env fn-name)
        env)
      state)))

(defn- match-defn-arity [^Defn {:keys [arities env]} args]
  (loop [[{:keys [params var-params] :as arity} & tail] arities
         best nil]
    (cond (nil? arity) best

          ;; exact match
          (and (= (count params) (count args)) (nil? var-params))
          arity

          :else (recur tail
                       (if (and var-params
                                (>= (count args) (count params))
                                (> (count (:params arity))
                                   (count (:params best))))
                         arity
                         best)))))

#_{:params [!a !b], :var-params !sq, :body [...]}

(defn- zip-params-args [{:keys [params var-params]} args]
  (let [pcount (count params)
        varargs (seq (drop pcount args))
        params* (cond-> params varargs (conj var-params))
        args* (cond-> (vec (take pcount args))  varargs (conj varargs))]
    (zipmap params* args*)))

#_(zip-params-args '{:params [!a !b], :var-params !sq, :body [...]} [1 2 3 4])

(defn execute-applicaiton [proc args state]
  (cond (core/primitive-procedure? proc) 
        (update state :ds conj (apply proc args))

        (instance? Continuation proc)
        (assoc state :cs (.cs ^Continuation proc))

        (instance? Defn proc)
        (let [^Arity arity (match-defn-arity proc args)
              env (core/extend-env (.env proc) (zip-params-args arity args))]
          ((.body arity)
           (assoc state :env env)))
        :else (throw (ex-info  "Unknown procedure type: "  
                              {:proc proc :args args}))))

(defn analyze-application [[_ op args-count]] ;; (invoke> + 2)
  (let [op-fn (analyze op)]
    (fn [{:keys [env ds] :as state}]
      (let [args-count (if (number? args-count) 
                         args-count
                         (core/lookup-variable-value env args-count))
            stack* (subvec ds 0 (- (count ds) args-count))
            args (take-last args-count ds)
            state* (assoc state :ds stack*)]
        (assoc (execute-applicaiton 
                 (-> (op-fn state*) :ds peek) 
                 args
                 state*)
               :env
               (:env state*))))))

(defn- def? [^String sym]
  (and (str/starts-with? sym "!") (str/ends-with? sym "+")))

(defn- swap [v] 
  (let [c (count v)
        i1 (dec c)
        i2 (dec i1)]
    (assoc v i2 (v i1) i1 (v i2))))

#trace
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
      (cond (= sym '<pop>) (fn [state] (update state :ds pop))
            (= sym '<swap>) (fn [state] (update state :ds swap))
            (= sym '<call>) (analyze-call sym)
            (= sym '<cc>) (fn [state] (update state :ds conj '<cc>))
            ;; to allow call/cc we have to be able to modify call stack
            (def? _name) (analyze-def sym)
            :else (analyze-lookup sym)))) 

  clojure.lang.ISeq
  (analyze [[op :as sexp]]
    (case op
      set! (analyze-assignment sexp) 
      def (analyze-def sexp)
      defn> (analyze-defn sexp)
      ; fn (analyze-fn sexp)
      call/cc> (analyze-call-cc sexp)
      if> (analyze-if sexp)
      quote> (analyze-quoted sexp) 
      quote (analyze-quoted sexp)
      ; let (analyze-let sexp)
      invoke> (analyze-application sexp)
      (analyze-sequence sexp))))

(comment
  
  
  (program->stack '(
                    
                    (defn> each
                      [!vc !quot]
                      (call/cc>
                        (quote>
                          !continue+
                          <pop>
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
                            else>))))

                    '(1 2 3)
                    (quote>
                      (invoke> !continue 0)
                      ">>> number: " 
                      <swap>
                      (invoke> println 2))
                    (invoke> each 2)

                    ))

  (program->stack '(
                    1
                    (call/cc>
                      (quote>
                        !c1+
                        <pop>
                        2
                        (call/cc> (quote> !c2+ 
                                          <pop> 
                                          (invoke> !c1 0)
                                          9 
                                          (invoke> !c2 0)))
                        3
                        ))
                    "end"
                    ))

  (program->stack '(
                    (defn> plus7 
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
                       (invoke> + !args-count)
                       ))
                    1 1 1 1
                    (invoke> plus7 4)
                    ))

  (program->stack '(
                  1 1
                  (invoke> = 2)
                  (if> "was true" 1 2 !c+ else> "was false")
                  ))

  (program->stack '(
                    (quote> 1 2 3 !c+)
                    <call>
                    ))

  (require '[flow-storm.api :as fs-api])
  (fs-api/local-connect)
  (fs-api/stop)

  )
