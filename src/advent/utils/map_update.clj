(ns advent.utils.map-update)

(defn update-map [f m]
  (reduce-kv
    (fn [m k v]
      (assoc m k (f v)))
    {}
    m))
