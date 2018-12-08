(ns advent.five
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [advent.utils.input :as io]))

(def input (first (io/read-input "five.input")))

(def test-input "dabAcCaCBAcCcaDA")

(defn react? [^Character a ^Character b]
  (and (or (Character/isUpperCase a) (Character/isUpperCase b))
       (or (Character/isLowerCase a) (Character/isLowerCase b))
       (= (str/upper-case a) (str/upper-case b))))

(deftest react?-test
  (is (true? (react? \c \C)))
  (is (true? (react? \C \c)))
  (is (false? (react? \c \c)))
  (is (false? (react? \c \a)))
  (is (false? (react? \A \c))))

(defn polymer-pairs->str [pairs]
  (apply
    str
    (concat
      (map
        (fn [[_ [a _]]] a)
        (butlast pairs))
      (-> pairs last second))))

(deftest polymer-pairs->str-test
  (is (= "daDD"
         (polymer-pairs->str {0 [\d \a]
                              1 [\a \D]
                              2 [\D \D]}))))

(defn polymer->pairs [polymer]
  (into (sorted-map) (zipmap (range) (partition 3 2 polymer))))

(deftest polymer->pairs-test
  (is (= {0 [\d \a]
          1 [\a \D]
          2 [\D \D]}
         (polymer->pairs "daDD"))))

(defn dissoc-reacting [pairs key]
  (dissoc pairs key (inc key)))

(comment
  (defn foo [polymer]
   (let [pairs (polymer->pairs polymer)]
     (reduce
       (fn [pairs [idx [a b]]]
         (if (react? a b)
           ))
       pairs
       pairs))))

(defn react-once [polymer]
  (let [pairs (polymer->pairs polymer)
        reaction-index (some (fn [[index [a b]]] (when (react? a b) index)) pairs)
        pairs (if reaction-index
                (dissoc-reacting pairs reaction-index)
                pairs)]
    (polymer-pairs->str pairs)))

(deftest react-once-test
  (is (= "dabAaCBAcCcaDA" (react-once "dabAcCaCBAcCcaDA"))))

(defn react [polymer]
  (loop [p polymer]
    (let [result (react-once p)]
      (if (= p result)
        result

        (recur result)))))

(deftest reactions-test
  (is (= "dabCBAcaDA"
         (react "dabAcCaCBAcCcaDA"))))

(defn step-one [input]
  (-> input react count))

(deftest step-one-test
  (is (= 10 (step-one test-input))))

(defn remove-unit [polymer ^Character unit]
  (apply str (remove #{(Character/toLowerCase unit) (Character/toUpperCase unit)} polymer)))

(defn step-two [input]
  (let [units (distinct input)
        polymers (into #{} (map (partial remove-unit input) units))]
    (apply min (map step-one polymers))))

(deftest step-two-test
  (is (= (step-two test-input) 4)))