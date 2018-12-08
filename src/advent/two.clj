(ns advent.two
  (:require [advent.utils.input :as io]
            [advent.utils.map-update :refer [update-map]]
            [clojure.test :refer :all]))

(def input (io/read-input "two.input"))

(def test-input-step-one
  ["abcdef"
   "bababc"
   "abbcde"
   "abcccd"
   "aabcdd"
   "abcdee"
   "ababab"])

(def test-input-step-two
  ["abcde"
   "fghij"
   "klmno"
   "pqrst"
   "fguij"
   "axcye"
   "wvxyz"])

(def interesting-keys #{3 2})

(defn step-one [input]
  (->> input
       (map frequencies)
       (map (partial group-by val))
       (map (partial update-map (constantly 1)))
       (apply merge-with +)
       (filter (comp interesting-keys key))
       (map second)
       (reduce *)))

(deftest step-one-test
  (is (= 12 (step-one test-input-step-one))))

(defn is-similar?
  ([a b] (is-similar? a b 1))
  ([a b limit]
   (and
     (= (count a) (count b))
     (->> a
         (map-indexed
           (fn [i char]
             (not= (nth b i) char)))
         (remove false?)
         count
         (>= limit)))))

(deftest is-similar?-test
  (is (true? (is-similar? "aaa" "aaa")))
  (is (true? (is-similar? "aaa" "aad")))
  (is (true? (is-similar? "aaa" "aba")))
  (is (false? (is-similar? "aaa" "add"))))

(defn similar-boxes-to [box others]
  (filter (partial is-similar? box) others))

(deftest similar-boxes-test
  (is (= ["aaa" "aba"] (similar-boxes-to "aaa" ["aaa" "cca" "aba"])))
  (is (= [] (similar-boxes-to "aaa" ["add" "cca" "kba"]))))

(defn similar-boxes [boxes]
  (loop [[box & rest] boxes]
    (let [similars (similar-boxes-to box rest)]
      (if (not-empty similars)
        (conj similars box)

        (recur rest)))))

(defn step-two [input]
  (let [[a b] (similar-boxes input)]
    (apply
      str
      (keep-indexed
       (fn [i char]
         (when (= (nth b i) char)
           char))
       a))))

(deftest step-two-test
  (is (= "fgij" (step-two test-input-step-two))))