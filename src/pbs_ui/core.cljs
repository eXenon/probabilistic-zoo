(ns pbs-ui.core)

(defn last-n-char [s n] (subs s (- (count s) n)))

(defn cut-last-n-char [s n] (subs s 0 (- (count s) n)))

(defn last-char [s] (last-n-char s 1))

(defn last-3-char [s] (last-n-char s 3))

(defn cut-last-char [s] (cut-last-n-char s 1))

(defn cut-last-3-char [s] (cut-last-n-char s 3))

(defn get-si-factor [s]
  (case s
    "h" 100
    "k" 1000
    "M" 1e6
    "G" 1e9
    "T" 1e12
    "P" 1e15
    "E" 1e18
    "Z" 1e21
    "Y" 1e24
    "R" 1e27
    "Q" 1e30
    1))

(defn get-binary-factor [s]
  (case s
    "KiB" 1024
    "MiB" 1048576
    "GiB" 1073741824
    "TiB" 1099511627776
    "EiB" 1125899906842624
    1))

(defn is-si-factor [s]
  (> 1 (get-si-factor s)))

(defn is-binary-factor [s]
  (> 1 (get-binary-factor s)))

(defn convert-si [value]
  (try
    (let [unit (last-char value)
          without-unit (cut-last-char value)
          si-factor (get-si-factor unit)]
      (if (is-si-factor unit)
        (* si-factor (js/parseFloat without-unit))
        (* si-factor (js/parseFloat value))))
    (catch js/Object e :error)))

(defn convert-binary [value]
  (try
    (let [unit (last-3-char value)
          without-unit (cut-last-3-char value)
          binary-factor (get-binary-factor unit)]
      (if (is-binary-factor unit)
        (* binary-factor (js/parseFloat without-unit))
        (* binary-factor (js/parseFloat value))))
    (catch js/Object e :error)))

(defn convert [value unittype]
  (case unittype
    :si (convert-si value)
    :binary (convert-binary value)
    :int (js/parseInt value)
    :float (js/parseFloat value)))

(defn set-error [id]
  (-> js/document
      (.getElementById id)
      (.-classList)
      (.add "error")))

(defn unset-error [id]
  (-> js/document
      (.getElementById id)
      (.-classList)
      (.remove "error")))

(defn read [id unittype]
  (let [v (-> js/document (.getElementById id) (.-value) (convert unittype))]
    (if (or (js/isNaN v) (= v :error))
      (set-error id)
      (unset-error id))
    v))

(defn write [id value]
  (-> js/document (.getElementById id) (.-value) (set! value)))

(defn write-span [id value]
  (-> js/document (.getElementById id) (.-innerText) (set! value)))

(defn to-p [n k m]
  (.evaluate js/math (str "round(pow(1 - exp(- " k " / ( " m " / " n ")), " k "), 15)")))

(defn to-inv-p [n k m]
  (.evaluate js/math (str "round(1 / pow(1 - exp(- " k " / ( " m " / " n ")), " k "))")))

(defn to-eps [w]
  (.evaluate js/math (str "e / " w "")))

(defn to-delta [d]
  (.evaluate js/math (str "1 - exp(- " d " )")))

(defn to-inv-delta [d]
  (.evaluate js/math (str "round(1 / (1 - exp(- " d " )))")))

(defn to-byte-size [v]
  (cond
    (< v 1e3) (str v "B")
    (< v 1e6) (str (.evaluate js/math (str "round(" v " / 1e3, 2)")) "kB")
    (< v 1e9) (str (.evaluate js/math (str "round(" v " / 1e6, 2)")) "MB")
    (< v 1e12) (str (.evaluate js/math (str "round(" v " / 1e9, 2)")) "GB")
    :else (str (.evaluate js/math (str "round(" v " / 1e12, 2)")) "TB")))

(defn ^:export refresh-p []
  (let [n (read "bloom-n" :si)
        k (read "bloom-k" :int)
        m (read "bloom-m" :binary)]
    (write "bloom-p" (to-p n k m))
    (write-span "bloom-p-human" (to-inv-p n k m))))

(defn ^:export refresh-cms []
  (let [w (read "cms-w" :si)
        d (read "cms-d" :si)
        epsilon (to-eps w)
        delta (to-delta d)]
    (write "cms-e" epsilon)
    (write "cms-delta" delta)
    (write-span "cms-size" (to-byte-size (* w d)))
    (write-span "cms-d-human" (to-inv-delta d))))

(refresh-p)
(refresh-cms)
