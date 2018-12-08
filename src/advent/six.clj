(ns advent.six
  (:require [clojure.test :refer :all]
            [advent.utils.input :as io]
            [advent.utils.map-update :refer [update-map]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn parse-input [input]
  (mapv
    (fn [s]
      (let [[x y] (str/split s #", ")]
        [(Integer. x) (Integer. y)]))
    input))

(def input (parse-input (io/read-input "six.input")))

(def test-input (parse-input
                  ["1, 1"
                  "1, 6"
                  "8, 3"
                  "3, 4"
                  "5, 5"
                  "8, 9"]))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs ^Integer (- x2 x1)) (Math/abs ^Integer (- y2 y1))))

(deftest manhattan-distance-test
  (is (= 2 (manhattan-distance [0 0] [1 1])))
  (is (= 0 (manhattan-distance [0 0] [0 0])))
  (is (= 1 (manhattan-distance [0 1] [0 0]))))

(defn board-dimensions [input]
  (let [min-x (apply min (map first input))
        min-y (apply min (map second input))
        max-x (apply max (map first input))
        max-y (apply max (map second input))]
    {:top-left [min-x min-y]
     :top-right [max-x min-y]
     :bottom-left [min-x max-y]
     :bottom-right [max-x max-y]}))

(deftest board-dimensions-test
  (is (= (board-dimensions [[0 0] [1 1] [5 5]])
         {:top-left [0 0]
          :top-right [5 0]
          :bottom-left [0 5]
          :bottom-right [5 5]})))

(defn board-coordinates
  ([input] (board-coordinates input 0))
  ([input extension]
   (combo/cartesian-product
     (range (- (apply min (map first input)) extension) (+ extension (inc (apply max (map first input)))))
     (range (- (apply min (map second input)) extension) (+ extension (inc (apply max (map second input))))))))

(deftest board-coordinates-test
  (is (= [[0 0] [0 1] [1 0] [1 1]]
         (board-coordinates [[1 1] [0 0] [1 0]]))))

(defn unique-closest [list-of-distances]
  (let [[[a-coord a-dist] [_ b-dist]] (sort-by second list-of-distances)]
    (when-not (= a-dist b-dist)
      a-coord)))

(defn closest-nodes [coords nodes]
  (into
    {}
    (map
      (fn [c]
        [c
         (unique-closest
           (map
            (fn [n]
              [n (manhattan-distance c n)])
            nodes))])
      coords)))

(defn nodes-by-size [distance-list]
  (as-> distance-list $
        (group-by val $)
        (update-map count $)
        (dissoc $ nil)))

(defn borders [input]
  (let [extended (board-coordinates input 1)
        coords (board-coordinates input)]
    (set/difference (set extended) (set coords))))

(deftest borders-test
  (is (= (borders [[0 0] [1 1]])
         #{'(-1 -1)
           '(-1 0)
           '(-1 1)
           '(-1 2)
           '(0 -1)
           '(0 2)
           '(1 -1)
           '(1 2)
           '(2 -1)
           '(2 0)
           '(2 1)
           '(2 2)})))

(defn infinites [input]
  (let [_ (println "Defining borders..")
        coords (borders input)
        _ (println "Board has" (count coords) "border coordinates")
        _ (println "Find the closest nodes for border regions..")
        closests (closest-nodes coords input)]
    (into #{} (keep val closests))))

(defn step-one [input]
  (let [;; koordinaattilista pöydälle
        _ (println "Defining coordinates..")
        coords (board-coordinates input)
        _ (println "Board contains" (count coords) "coordinates")
        ;; laske jokaiselle koordinaatille lähimmät nodet
        _ (println "Calculating closest nodes..")
        closests (closest-nodes coords input)
        _ (println "Calculated closest nodes")
        ;; järjestä nodet suurimmasta pienimpään
        _ (println "Order nodes by size..")
        by-size (nodes-by-size closests)
        _ (println "Sorted by size")
        ;; käy reunoilla olevat koordinaatit läpi, ja poista nodet joille löytyy täältä solu
        _ (println "Finding infinite nodes..")
        non-infinites (remove (comp (infinites input) key) by-size)
        _ (println "Finding the non-infinite node with most cells")]
    (apply max (map second non-infinites))))

(deftest step-one-test
  (is (= 17 (step-one test-input))))

(defn step-two [region-size input]
  (let [coords (board-coordinates input)
        closests (->>
                   (map
                    (fn [c]
                      [c
                       (reduce +
                               (map
                                 (fn [n]
                                   (manhattan-distance c n))
                                 input))])
                    coords)
                   (filter (comp (partial > region-size) second)))]
    (count closests)))

(deftest step-two-test
  (is (= 16 (step-two 32 test-input))))