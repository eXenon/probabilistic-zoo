(ns pbs.core
  (:require [clj-commons.digest :as d]
            [pbs.bloom :as bloom]
            [pbs.cmsketch :as cmsketch]
            [pbs.util :refer [hex->bytes, bytes->hex, inc-if]]))

(def inputs (map (comp hex->bytes d/sha1 str) (range)))

(def str-inputs (map (comp d/sha1 str) (range)))

(def repeated-inputs (map (comp d/sha1 str) (flatten (map #(take % (repeat %)) (range)))))

(defn simulator-bloom [number-of-elts size number-of-hashes number-of-sims]
  (let [bf (bloom/bloom size number-of-hashes)
        bf-inputs (reduce bloom/bloom-add bf (take number-of-elts inputs))]
    (println "Starting Bloom Filter simulation: N=" number-of-elts " m="  size " k=" number-of-hashes)
    (println "Probability of error: " (bloom/bloom-error-probability bf-inputs number-of-elts))
    (loop [elts (take number-of-sims (drop number-of-elts inputs))
           true-negative 0
           false-positive 0]
      (let [elt (take 1 elts)
            is-false-positive? (bloom/bloom-contains? bf-inputs elt)
            is-true-negative? (not is-false-positive?)
            is-fp-msg (if is-false-positive? "False positive!" "True negative.")
            new-tn (inc-if is-true-negative? true-negative)
            new-fp (inc-if is-false-positive? false-positive)
            error-rate (format "%.2f" (float (* 100 (/ new-fp (+ new-fp new-tn)))))]
        (if (> (count elts) 0)
          (do
            (println "Added" (+ new-tn new-fp) "elements, found error rate of" error-rate "%")
            (recur (drop 1 elts)
                   new-tn
                   new-fp)))))))

(defn simulator-cmsketch [width depth number-of-elts number-of-sims]
  (let [cms (cmsketch/cmsketch width depth)
        cms-inputs (reduce cmsketch/cmsketch-add cms (take number-of-elts repeated-inputs))
        cms-probs (cmsketch/cmsketch-error-probability cms)
        real-counts (frequencies (take number-of-elts repeated-inputs))
        ε (:epsilon cms-probs)
        δ (:delta cms-probs)
        t (* number-of-sims ε)]
    (println "Starting CMSketch simution: N=" number-of-elts " w=" width " d=" depth)
    (println "It is expected that the result will be off by " t " with a probability of " (- 1 δ))
    (doseq [elt (take number-of-sims str-inputs)]
      (let [cms-count (cmsketch/cmsketch-count cms-inputs elt)
            real-count (get real-counts elt 0)
            d (- cms-count real-count)]
        (if (= cms-count real-count)
          (println "Counted" cms-count "which is correct!")
          (println "Counted" cms-count "instead of" real-count ", error is" d)))))

  nil)

(defn simulator [ds]
  (case ds
    :bloom (simulator-bloom 100 256 6 500)))

(defn -main [args]
  (print "hello world"))

(comment
  (simulator-bloom 100 256 6 500)
  (simulator-cmsketch 10 10 100 100))
