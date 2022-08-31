(ns dumch.impl.stackbased-call-cc
  (:require
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

;; returns fn: State -> State
(defprotocol IAnalyze (analyze [sexp]))

(defn -eval [sexp state]
  ((analyze sexp) state))

(def inf-prot (atom 0))

(defn eval-program [{[h & tail] :cs :as state}]
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
        conseq-fn (analyze-sequence conseq)
        alt-fn (analyze-sequence alt)]
    (fn [{:keys [ds] :as state}]
      (let [pred (peek ds)
            state* (-> (update state :ds pop)
                       (update :env core/extend-env))
            r (if (true? pred)
                (conseq-fn state*)
                (alt-fn state))]
        r))))

(defn analyze-call [_]
  (fn [{:keys [ds] :as state}]
    (let [sq (peek ds)
          state* (-> (update state :ds pop)
                     (update :env core/extend-env))]
      ((analyze-sequence sq) state*))))

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

(defn execute-applicaiton [proc args state]
  (cond (core/primitive-procedure? proc) 
        (update state :ds conj (apply proc args))

        (instance? Continuation proc)
        (assoc state :cs (.cs ^Continuation proc))

        (core/compound-procedure? proc)
        (let [proc ^Proc proc] 
          ((.body proc)
           (update state :env (core/extend-env (.params proc) args (.env proc)))))
        :else (throw (ex-info  "Unknown procedure type: "  
                              {:proc proc :args args}))))

(defn analyze-application [[_ op args-count]] ;; (invoke> + 2)
  (let [op-fn (analyze op)]
    (fn [{:keys [env ds] :as state}]
      (let [stack* (subvec ds 0 (- (count ds) args-count))
            args (take-last args-count ds)
            state* (assoc state :ds stack*)]
        (execute-applicaiton 
                     (-> (op-fn state*) :ds peek) 
                     args
                     state*)))))

(defn- def? [^String sym]
  (and (str/starts-with? sym "!") (str/ends-with? sym "+")))

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
      ; defn (analyze-defn sexp)
      ; fn (analyze-fn sexp)
      call/cc> (analyze-call-cc sexp)
      if> (analyze-if sexp)
      quote> (analyze-quoted sexp) 
      quote (analyze-quoted sexp)
      ; let (analyze-let sexp)
      invoke> (analyze-application sexp))))

(comment
  
  (program->stack '(
                    1
                    (call/cc>
                      (quote>
                        !c1+
                        <pop>
                        2
                        (call/cc> (quote> !c2+ <pop> (invoke> !c1 0) 9))
                        3
                        ))
                    "end"
                    ))

  (program->stack '(
                  1 1
                  (invoke> = 2)
                  (if> "was true" 1 2 else> "was false")
                  (quote> 1 2 (invoke> + 2))
                  <call>
                  ))


  (require '[flow-storm.api :as fs-api])
  (fs-api/local-connect)
  (fs-api/stop)

  )
