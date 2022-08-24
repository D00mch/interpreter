(ns dumch.interpreter-test
  (:require [clojure.test :refer [deftest testing is]]
            [dumch.impl.direct :as direct :refer [eval-program]]
            [dumch.impl.nondeterministic :as nondeterministic]))

(defn primitives-test [eval-program]
  (testing "primitives and self-evaluating"
    (is (= 1 (eval-program '(1))))
    (is (= true (eval-program '(true))))
    (is (= false (eval-program '(false))))
    (is (= \a (eval-program '(\a))))
    (is (= "self" (eval-program '("self"))))
    (is (= nil (eval-program '(nil))))
    (is (= '(1 2 3) (eval-program '('(1 2 3)))))
    (is (= [1] (eval-program '([1])))))

  (testing "primitive procedures"
    (is (= 1 (eval-program '((+ (* 1 1 1) 0)))))
    (is (= 2 (eval-program '((count [0 0])))))
    (is (= true (eval-program '((nil? (seq [])))))))

  (testing "compound procedures"
    (is (eval-program '(
                        (defn even? [x]
                          (defn _even? [n]
                            (if (= n 0)
                              true
                              (_odd? (- n 1))))
                          (defn _odd? [n]
                            (if (= n 0)
                              false
                              (_even? (- n 1))))
                          (_even? x))
                        (even? 4)))))

  (testing "anonymous lambdas"
    (is (eval-program '((= 1 
                           ((fn [a] a) 1)
                           ((fn [a b] (* a b)) 1 1))))))

  (testing "defines and assignments"
    (is (= '(true 2)
           (eval-program '((def a 2)
                           (defn test []
                             (def a 2)
                             (def b 3)
                             (set! a 1)
                             (* a a a))
                           (cons (= (test) 1)
                                 (cons a '())))))))

  (testing "recurtion"
    (is (= 24 (eval-program '((defn factorial [n]
                                (if (= n 1)
                                  1
                                  (* n (factorial (- n 1))))) 
                              (factorial 4)))))))

(deftest direct-interpreter
  (primitives-test dumch.impl.direct/eval-program))

(deftest nondeterministic-interpreter
  (primitives-test dumch.impl.nondeterministic/eval-program))
