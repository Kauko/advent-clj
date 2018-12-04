(ns advent.utils.input
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input [filename]
  (some-> filename
          io/resource
          slurp
          str/split-lines))

(defn as-numbers [filename]
  (map #(Integer. %) (read-input filename)))
