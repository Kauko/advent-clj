(ns advent.nine
  (:require [clojure.test :refer :all]
            [advent.utils.input :as io]
            [clojure.string :as str]
            [advent.utils.ringlist :as rl]))

(def something-entirely-different-multiple 23)

(defn parse [input]
  (let [[players _ _ _ _ _ points] (str/split input #" ")]
    {:players (Integer. players)
     :end-at (Integer. points)}))

(def input (-> "nine.input" io/read-input first))

(def test-inputs {["9 players; last marble is worth 25 points"] 32
                  ["10 players; last marble is worth 1618 points"] 8317
                  ["13 players; last marble is worth 7999 points"] 146373
                  ["17 players; last marble is worth 1104 points"] 2764
                  ["21 players; last marble is worth 6111 points"] 54718
                  ["30 players; last marble is worth 5807 points"] 37305})

(def test-input (-> test-inputs first first first parse))

(comment
  ;; Vectors worked for the first step, but finishing step two would've taken
  ;; +15h.
  (defn print-table! [current-player current-marble-index marbles-on-table]
   (println current-player ":" (str/join
                                 " "
                                 (map-indexed
                                   (fn [index marble]
                                     (if (= index current-marble-index)
                                       (str "(" marble ")")
                                       (str marble)))
                                   marbles-on-table))))

  (defn calculate-scores* [players end-at]
    #_(println "- : (0)")
    (loop [current-marble-index 0
           [marble-to-place & remaining-marbles] (rest (range))
           marbles-on-table [0]
           [current-player & player-turns] (cycle (range players))
           scores (zipmap (range players) (repeat 0))]
      (let [marbles-count (count marbles-on-table)]
        (if (> marble-to-place end-at)
          scores

          (if (zero? (mod marble-to-place something-entirely-different-multiple))
            (let [remove-index (mod (- current-marble-index 7) marbles-count)
                  [before [marble-to-remove & after]] (split-at remove-index marbles-on-table)
                  points (+ marble-to-place marble-to-remove)]
              (do
                #_(println "Player" current-player "places down" marble-to-place ", scoring" marble-to-place "+" marble-to-remove "=" points "!")
                (do
                  #_(print-table! current-player remove-index (vec (concat before after)))
                  (recur
                    remove-index
                    remaining-marbles
                    (vec (concat before after))
                    player-turns
                    (update scores current-player + points)))))

            (let [insert-index (mod (+ 2 current-marble-index) marbles-count)]
              (if (= insert-index 0)
                (do
                  #_(print-table! current-player marbles-count (conj marbles-on-table marble-to-place))
                  (recur
                    marbles-count
                    remaining-marbles
                    (conj marbles-on-table marble-to-place)
                    player-turns
                    scores))

                (let [[before after] (split-at insert-index marbles-on-table)]
                  (do
                    #_(print-table! current-player insert-index (vec (concat before (conj after marble-to-place))))
                    (recur
                      insert-index
                      remaining-marbles
                      (vec (concat before (conj after marble-to-place)))
                      player-turns
                      scores)))))))))))

(defn calculate-scores [players end-at]
  (loop [board (rl/->ringlist 0)
         [marble-to-place & remaining-marbles] (rest (range))
         [current-player & player-turns] (cycle (range players))
         scores (zipmap (range players) (repeat 0))]
    #_(print current-player ": ")
    #_(rl/print-values board)
    (if (> marble-to-place end-at)
      scores

      (if (zero? (mod marble-to-place something-entirely-different-multiple))
        (let [board (rl/rewind board 7)
              marble-to-remove (:active-node-value board)
              board (rl/remove-active-node board)]
          (do
           #_(println "Player" current-player "places down" marble-to-place ", scoring" marble-to-place "+" marble-to-remove "!")
           (recur
             board
             remaining-marbles
             player-turns
             (update scores current-player + marble-to-place marble-to-remove))))

        (recur
          (-> board (rl/insert-value-after-steps marble-to-place 1))
          remaining-marbles
          player-turns
          scores)))))

(defn step-one [input]
  (let [{:keys [players end-at]} (parse input)
        scores (calculate-scores players end-at)]
    (apply max (vals scores))))

(deftest step-one-test
  (is (= (step-one (-> ["9 players; last marble is worth 25 points"] first)) 32))
  (is (= (step-one (-> ["10 players; last marble is worth 1618 points"] first)) 8317))
  (is (= (step-one (-> ["13 players; last marble is worth 7999 points"] first)) 146373))
  (is (= (step-one (-> ["17 players; last marble is worth 1104 points"] first)) 2764))
  (is (= (step-one (-> ["21 players; last marble is worth 6111 points"] first)) 54718))
  (is (= (step-one (-> ["30 players; last marble is worth 5807 points"] first)) 37305)))

(defn step-two [input]
  (let [{:keys [players end-at]} (parse input)
        scores (calculate-scores players (* 100 end-at))]
    (apply max (vals scores))))