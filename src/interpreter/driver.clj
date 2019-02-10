(ns interpreter.driver
  (:require [interpreter.type :refer [initial-state]]))

(def ^:private input-prompt "write your code below")
(def ^:private output-prompt ">> ")
(def ^:private prompt-for-input #(println %))

(defn- user-print [object]
  (println output-prompt object))

(defn print-loop
  "reduce-state-fn should be fn, that take State and sexps
  and return State as a result of evaluating sexps"
  [reduce-state-fn]
  (loop [state initial-state]
    (prompt-for-input input-prompt)
    (let [input  (read-string (read-line))
          output (reduce-state-fn state (list input))]
      (user-print (:result output))
      (recur output))))