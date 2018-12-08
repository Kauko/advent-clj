(ns advent.seven
  (:require [advent.utils.input :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test :refer :all]))

(defn str->map [instruction]
  (let [instruction (str/split instruction #" ")]
    {(nth instruction 1) #{(nth instruction 7)}}))

(defn parse-input [input]
  (->> input
       (map str->map)))

(def input (parse-input (io/read-input "seven.input")))

(def test-input (parse-input
                  ["Step C must be finished before step A can begin."
                   "Step C must be finished before step F can begin."
                   "Step A must be finished before step B can begin."
                   "Step A must be finished before step D can begin."
                   "Step B must be finished before step E can begin."
                   "Step D must be finished before step E can begin."
                   "Step F must be finished before step E can begin."]))

(def durations
  (zipmap
    (map
      (comp str char)
      (range (int \A) (inc (int \Z))))
    (map inc (range))))

(defn step-one [input]
  (let [input (apply (partial merge-with set/union) input)
        starting-steps (sort (into #{} (remove (into #{} (apply concat (vals input))) (keys input))))]
    (apply
      str
      (loop [possible-steps starting-steps
             steps-taken []
             input input]
        (if (empty? possible-steps)
          steps-taken

          (let [step (some
                       (fn [s]
                         (when (every? (fn [[_ v]] (not (v s))) input) s))
                       (sort possible-steps))]
            (recur (concat (remove (partial = step) possible-steps) (get input step))
                   (conj steps-taken step)
                   (dissoc input step))))))))

(deftest step-one-test
  (is (= "CABDFE" (step-one test-input))))

(defn step-two [workers duration input]
  (let [input (apply (partial merge-with set/union) input)
        workers (map #(do [% []]) (range workers))
        starting-steps (sort (into #{} (remove (into #{} (apply concat (vals input))) (keys input))))]
    (loop [seconds 0
           possible-steps starting-steps
           workers workers
           input input
           steps-taken []]
      (if (and (empty? possible-steps) (= (count workers)
                                          (count (filter (comp empty? second) workers))))
        seconds

        (let [finished-steps (keep (fn [[_ [step finished-at]]] (when (= finished-at seconds) step)) workers)
              workers (map (fn [[worker [_ finished-at] :as w]]
                             (if (= finished-at seconds)
                               [worker []]
                               w))
                           workers)
              free-workers (filter (comp empty? second) workers)
              possible-steps (concat possible-steps (mapcat (fn [s] (get input s)) finished-steps))
              new-input (apply dissoc input finished-steps)
              steps-to-take (filter
                              (fn [s]
                                (when (every? (fn [[_ v]] (not (v s))) new-input) s))
                              (sort (into #{} possible-steps)))]
          (println seconds)
          (println finished-steps)
          (println workers)
          (println possible-steps)
          (println new-input)
          (println steps-to-take)
          (println steps-taken)
          (println)
          (recur (inc seconds)
                 (drop (count free-workers) steps-to-take)
                 (map
                   (fn [[w task] s]
                     (cond
                       (and (empty task) s)
                       [w [s (+ seconds duration (durations s))]]

                       (not-empty task)
                       [w task]

                       (not s)
                       [w []]))
                   workers
                   (concat steps-to-take (repeat nil)))
                 new-input
                 (concat steps-taken steps-to-take)))))))

(defn step-two [workers duration input]
  (let [input (apply (partial merge-with set/union) input)
        workers (map #(do [% []]) (range workers))
        starting-steps (sort (into #{} (remove (into #{} (apply concat (vals input))) (keys input))))]
    (println "S\t"
             (clojure.string/join "\t" (map (fn [[id]] (str id)) workers))
             "\tFinished"
             "\tPossible steps"
             "\tUnsolved")
    (println "-------------")
    (dec
      (loop [seconds 0
             workers workers
             possible-steps starting-steps
             input input
             completed []]
        (if (and (empty? possible-steps) (= (count workers)
                                            (count (filter (comp empty? second) workers))))
          seconds

          (letfn [(free-workers [w] (filter (comp empty? second) w))
                  (busy-workers [w] (remove (comp empty? second) w))]
            (let [finished-this-turn (keep (fn [[_ [step finished-at]]] (when (= finished-at seconds) step)) workers)
                  workers (map (fn [[id [_ finished-at] :as w]] (if (= finished-at seconds) [id []] w)) workers)
                  possible-steps (sort (into #{} (concat possible-steps (mapcat (fn [s] (get input s)) finished-this-turn))))
                  updated-input (apply dissoc input finished-this-turn)
                  steps-to-take (->> possible-steps
                                     (filter
                                       (fn [s]
                                         (when (every? (fn [[_ v]] (not (v s))) updated-input) s)))
                                     sort
                                     (take (count (free-workers workers)))
                                     (into #{}))
                  updated-workers (concat
                                    (busy-workers workers)
                                    (map
                                      (fn [[id task :as worker] step]
                                        (if (and (empty? task) (some? step))
                                          [id [step (+ seconds duration (durations step))]]

                                          worker))
                                      (free-workers workers)
                                      (concat steps-to-take (repeat nil))))]
              (println seconds "\t"
                       (clojure.string/join "\t" (map (fn [[_ [step]]] (str (or step "."))) updated-workers))
                       "\t" (concat completed finished-this-turn)
                       "\t" possible-steps
                       "\t" (keys updated-input))
              (recur (inc seconds)
                     updated-workers
                     (remove steps-to-take possible-steps)
                     updated-input
                     (concat completed finished-this-turn)))))))))

(deftest step-two-test
  (is (= 15 (step-two 2 0 test-input))))