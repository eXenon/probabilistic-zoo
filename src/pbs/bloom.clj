(ns pbs.bloom
  (:require [clj-commons.digest :as d]
            [clojure.math :as math]
            [pbs.util :refer [hex->bytes, set-bit, is-bit-set?]]))

(defn to-n-bytes
  [elt]
  (let [hashed (hex->bytes (d/sha1 elt))]
    (lazy-seq (concat hashed (to-n-bytes hashed)))))

(defn hashi [elt m i]
  (loop [l i
         acc elt]
    (if (<= l 1)
      (mod (reduce + (hex->bytes (d/sha-256 acc))) m)
      (recur (- l 1) (d/sha-256 acc)))))

(defn bloom-error-probability [bf n]
  (let [k (:k bf)
        m (count (:filter bf))]
    (math/pow (- 1 (math/exp (/ (- k) (/ m n)))) k)))

(defn bloom-get-bits [bf elt]
  (let [size (count (:filter bf))
        k (:k bf)]
    (map #(hashi elt size (inc %)) (range k))))

(defn bloom [size number-of-hash-functions]
  {:filter (byte-array size)
   :k number-of-hash-functions})

(defn bloom-add [bf elt]
  (let [bits (bloom-get-bits bf elt)]
    (update bf :filter #(reduce set-bit % bits))))

(defn bloom-contains? [bf elt]
  (let [bits (bloom-get-bits bf elt)]
    (every? true? (map #(is-bit-set? (:filter bf) %) bits))))

(comment
  (def elt "deadbeef")
  (def b (bloom 20 6))
  (def b2 (bloom-add b elt))

  (bloom-contains? b elt)
  (bloom-contains? b2 elt))
