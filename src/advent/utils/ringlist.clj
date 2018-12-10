(ns advent.utils.ringlist
  (:require [clojure.string :as str]))

(defn ->node [value]
  {:value value
   :next nil
   :prev nil})

(defn ->ringlist [value]
  (let [value (or value 0)
        node (->node value)]
    {:active-node-value value
     value (assoc node
             :next value
             :prev value)}))

(defn active-node [this]
  (get this (:active-node-value this)))

(defn take-steps [this direction steps]
  (last
    (take
     steps
     (rest
       (iterate
         (fn [node]
           (get this (direction node)))
         (active-node this))))))


(defn insert-node-after [this insert after]
  (let [insert (assoc insert :prev (:value after)
                             :next (get-in this [(:next after) :value]))
        after (assoc after :next (:value insert))]
    (-> this
        (assoc (:value insert) insert
               (:value after) after)
        (assoc-in [(:next insert) :prev] (:value insert)))))

(defn insert-value-after-steps [this value steps]
  (let [active-node (take-steps this :next steps)
        new-node (->node value)]
    (-> this
      (insert-node-after new-node active-node)
      (assoc :active-node-value (:value new-node)))))

(defn rewind [this steps]
  (let [active-node (take-steps this :prev steps)]
    (assoc this :active-node-value (:value active-node))))

(defn remove-active-node [this]
  (let [node (active-node this)]
    (-> this
        (assoc-in [(:prev node) :next] (:next node))
        (assoc-in [(:next node) :prev] (:prev node))
        (assoc-in [(:value node) :prev] nil)
        (assoc-in [(:value node) :next] nil)
        (assoc :active-node-value (:next node)))))

(defn print-values [this]
  (let [start (active-node this)]
    (println
      (str/join
        " "
        (loop [v []
               node start]
          (let [v (conj v (:value node))
                next (get this (:next node))]
            (if (= next start)
              v

              (recur v next)))))))

  this)