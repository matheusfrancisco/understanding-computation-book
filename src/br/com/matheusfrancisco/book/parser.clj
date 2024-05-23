(ns br.com.matheusfrancisco.book.parser)

(defrecord Scanner [source tokens start current line])

(defn end-of-file? [scanner]
  (>= (:current scanner) (count (:source scanner))))

(defn scan-token [scanner])

(defn scan-tokens [scanner]

  (if (end-of-file? scanner)
    false
    (let [scanner (assoc scanner
                         :start (:current scanner))
          [token scanner] (scan-token scanner)]
      scanner)))

(def scanner (->Scanner
              "if (a > b) { x = 1; } else { y = 2; } "
              []
              0
              0
              1))

(scan-tokens scanner)
