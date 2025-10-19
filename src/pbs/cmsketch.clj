(ns pbs.cmsketch
  (:require [pbs.util :refer [iterated-hash-to-mod-int, update-at, get-at]]
            [clojure.math :refer [exp]]))

(defn cmsketch [width depth]
  {:cmsketch (map (fn [_] (map (constantly 0) (range width))) (range depth))})

(defn cmsketch-add [cms elt]
  (let [data (:cmsketch cms)
        depth (count data)
        width (count (first data))
        bits (map #(iterated-hash-to-mod-int elt width %) (range depth))
        inner (fn [b r] (update-at b inc r))]
    (update cms :cmsketch #(map inner bits %))))

(defn cmsketch-count [cms elt]
  (let [data (:cmsketch cms)
        depth (count data)
        width (count (first data))
        bits (map #(iterated-hash-to-mod-int elt width %) (range depth))]
    (apply min (map #(first (get-at % %2)) bits data))))

(defn cmsketch-error-probability [cms]
  (let [data (:cmsketch cms)
        depth (count data)
        width (count (first data))
        e 2.71828
        ε (/ e width)
        δ (/ 1 (exp depth))]
    {:epsilon ε :delta δ}))

(comment
  (def cms (cmsketch 15 15))
  (def cms2 (cmsketch-add cms "deadbeef"))

  cms2
  (cmsketch-count cms2 "deadbeef")
  (cmsketch-count cms2 "aaaaa"))
