(ns pbs.core
  (:require [clj-commons.digest :as d]
            [pbs.bloom :as bloom]
            [pbs.util :refer [hex->bytes, bytes->hex]]))

(def inputs (map (comp hex->bytes d/sha1 str) (range)))

(defn simulator-bloom [number-of-elts size number-of-hashes number-of-sims]
  (let [bf (bloom/bloom size number-of-hashes)
        bf-inputs (reduce bloom/bloom-add bf (take number-of-elts inputs))]
    (println "Starting Bloom Filter simulation: N=" number-of-elts " m="  size " k=" number-of-hashes)
    (println "Probability of error: " (bloom/bloom-error-probability bf-inputs number-of-elts))
    (doseq [elt (take number-of-sims (drop number-of-elts inputs))]
      (let [is-in-set? (bloom/bloom-contains? bf-inputs elt)
            is-false-positive? (not is-in-set?)
            is-fp-msg (if is-false-positive? "False positive!" "True negative.")]
        (println "Checking if" (bytes->hex elt) "is part of the set?" is-in-set? is-fp-msg)))))

(defn simulator [ds]
  (case ds
    :bloom (simulator-bloom)))

(defn -main [args]
  (print "hello world"))

(comment
  (simulator-bloom 100 256 6 50))
