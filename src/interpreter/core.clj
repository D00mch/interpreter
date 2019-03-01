(ns interpreter.core
  (:require [interpreter.impl.default :as default])
  (:gen-class))

(defmulti reduce-state :interpreter)

(defmethod reduce-state "default" [_] default/reduce-state)
(defmethod reduce-state "nondeterministic" [_] "TODO")

(defn -main
  "Pass implementation: default or nondeterministic"
  [& args]
  (println "Starting interpreter")
  )
