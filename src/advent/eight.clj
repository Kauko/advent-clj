(ns advent.eight
  (:require [clojure.test :refer :all]
            [advent.utils.input :as io]
            [clojure.string :as str]))

(defn ->numbers [input]
  (let [strings (-> input
                    first
                    (str/split #" "))]
    (map #(Integer. %) strings)))

(def input (->numbers (io/read-input "eight.input")))

(def test-input (->numbers ["2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"]))

(defn ->tree [{:keys [children metadata license]}]
  (let [[child-count meta-count & rest] license]
    (if (= 0 child-count)
      {:metadata (vec (take meta-count rest))
       :license (vec (drop meta-count rest))
       :value (reduce + (take meta-count rest))}

     (let [children (vec (take child-count (drop 1 (iterate ->tree {:license rest}))))
           licenses (mapv :license children)
           shortest-license (first (sort licenses))
           metadata (vec (take meta-count shortest-license))]
       {:children children
        :license (vec (drop meta-count shortest-license))
        :metadata metadata
        :value (reduce + (map :value (keep (fn [idx] (get children (dec idx))) metadata)))}))))

(defn metadata [tree]
  (->> tree
       (tree-seq (comp not-empty :children) :children)
       (mapcat :metadata)
       (reduce +)))

(defn step-one [input]
  (let [tree (->tree {:license input})]
    (metadata tree)))

(deftest step-one-test
  (is (= 138 (step-one test-input))))

(defn step-two [input]
  (-> {:license input} ->tree :value))

(deftest step-two-test
  (is (= 66 (step-two test-input))))