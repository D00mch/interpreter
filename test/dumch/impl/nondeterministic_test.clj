(ns dumch.impl.nondeterministic-test
  (:require [clojure.test :refer [deftest testing is]]
            [dumch.impl.nondeterministic :refer [eval-program]]))

(deftest amb-require-test
  (testing "basic amb"
    (is (eval-program '(
                        (defn require [p]
                          (if (not p) (amb) -1))

                        (defn an-element-of [items]
                          (require (some? (seq items)))
                          (amb (first items)
                               (an-element-of (next items))))

                        (= 1 (an-element-of [1 2 3])))))
    (is (eval-program '(
                        (let [a (amb 1 2 3)]
                          (if (< a 3) (amb))
                          a
                          (= a 3)))))))
