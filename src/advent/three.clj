(ns advent.three
  (:require [clojure.test :refer :all]
            [advent.utils.input :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn parse-plan [plan]
  (let [[id _ pos size] (str/split plan #" ")
        [left top] (str/split pos #",")
        [wide tall] (str/split size #"x")]
    {:id (Integer. (apply str (rest id)))
     :from-left (Integer. left)
     :from-top (Integer. (apply str (butlast top)))
     :width (Integer. wide)
     :height (Integer. tall)}))

(deftest parse-plan-test
  (is (= (parse-plan "#123 @ 3,2: 5x4")
         {:id 123
          :from-left 3
          :from-top 2
          :width 5
          :height 4})))

(defn plan->coordinates [{:keys [from-left from-top width height]}]
  (set
    (combo/cartesian-product
      (range from-left (+ from-left width))
      (range from-top (+ from-top height)))))

(deftest plan->coordinates-test
  (is (= #{'(5 5) '(5 6) '(6 5) '(6 6)}
         (plan->coordinates (parse-plan "#3 @ 5,5: 2x2")))))

(defn with-coordinates [plan]
  (assoc plan :coordinates (plan->coordinates plan)))

(defn parse-plans [plans]
  (map parse-plan plans))

(def input (parse-plans (io/read-input "three.input")))

(def test-input-step-one
  (parse-plans ["#1 @ 1,3: 4x4"
                "#2 @ 3,1: 4x4"
                "#3 @ 5,5: 2x2"]))

(deftest input-test
  (is (= (count (io/read-input "three.input"))
         (count (parse-plans (io/read-input "three.input"))))))

(defn overlapping-coordinates [a b]
  (set/intersection a b))

(defn overlaps? [a b]
  (not-empty (overlapping-coordinates a b)))

(defn step-one [input]
  (let [total (count input)]
    (loop [[plan & plans :as left] (map plan->coordinates input)
           shared-coords #{}]
      (println (- total (count left)) "/" total ": Shared slots" (count shared-coords))
      (if (nil? plan)
        (count shared-coords)

        (let [shared (mapcat (partial overlapping-coordinates plan) plans)]
          (recur plans (into shared-coords shared)))))))

(deftest step-one-test
  (is (= 4 (step-one test-input-step-one))))

(defn drop-nth [n coll]
  (keep-indexed
    (fn [i e]
      (when (not= i n) e))
    coll))

(defn step-two [input]
  (let [total (count input)
        all-plans (map with-coordinates input)]
    (loop [i 0]
      (if (>= i (count all-plans))
        nil

        (let [plan (nth all-plans i)
              plans (drop-nth i all-plans)]
          (println i "/" (dec total))
          (let [overlapping (filter (comp (partial overlaps? (:coordinates plan)) :coordinates) plans)]
            (if (= (count overlapping) 0)
              (:id plan)

              (recur (inc i)))))))))

(deftest step-two-test
  (is (= 3 (step-two test-input-step-one))))




