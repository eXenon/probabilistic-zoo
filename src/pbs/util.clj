(ns pbs.util)

(defn hex->bytes [s]
  (let [shifted (take-nth 2 (drop 1 s))
        normal (take-nth 2 s)]
    (byte-array (map #(Integer/parseInt (str % %2) 16) normal shifted))))

(defn bytes->hex [ba]
  (apply str
         (map #(format "%02x" %) ba)))

(defn set-bit [ba pos]
  (let [inner (fn [idx b]
                (let [bit-idx (* idx 8)
                      is-pos-in-current-byte (and (>= pos bit-idx) (< (- pos bit-idx) 8))]
                  (if is-pos-in-current-byte
                    (bit-or b (bit-shift-left 1 (mod pos 8)))
                    b)))]
    (byte-array (map-indexed inner ba))))

(defn is-bit-set? [ba pos]
  (let [byte- (aget ba (quot pos 8))]
    (not= 0 (bit-and byte- (bit-shift-left 1 (mod pos 8))))))

(defn inc-if [c v] (if c (inc v) v))

(comment
  (set-bit (byte-array 3) 15)
  (def b (set-bit (byte-array 5) 15))
  b
  (aget b 3)
  (is-bit-set? b 2)
  (is-bit-set? b 15))
