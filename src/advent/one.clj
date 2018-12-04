(ns advent.one
  (:require [advent.utils.input :as io]
            [clojure.test :refer :all]))

(def input (io/as-numbers "one.input"))

(defn step-one [input]
  (reduce + 0 input))

(deftest step-one-test
  (is (= 3 (step-one [1 1 1])))
  (is (= 0 (step-one [1 1 -2])))
  (is (= -6 (step-one [-1 -2 -3]))))

(defn step-two [input]
  (reduce
    (fn [[current-state old-states] change]
      (let [new-state (+ current-state change)]
        (if (old-states new-state)
          (reduced new-state)

          [new-state (conj old-states new-state)])))
    [0 #{0}]
    (cycle input)))

(deftest step-two-test
  (is (= 0 (step-two [+1 -1])))
  (is (= 10 (step-two [+3, +3, +4, -2, -4])))
  (is (= 5 (step-two [-6, +3, +8, +5, -6])))
  (is (= 14 (step-two [+7, +7, -2, -7, -4]))))