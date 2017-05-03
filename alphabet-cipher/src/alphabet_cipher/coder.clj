(ns alphabet-cipher.coder)

(def key-count 26)

(def alphabet (vec (take key-count (iterate (comp char inc int) \a))))

(def cycled-alphabet
  (map
    #(apply str
            (drop % (take (+ key-count %) (cycle alphabet))))
    (range key-count)))

(def chart
  (into {} (map vector alphabet cycled-alphabet)))

(defn format-keyword [word message]
  (apply str (take (count message) (cycle word))))

(defn column [a]
  (.indexOf alphabet a))

(def row (comp vec (partial chart)))

(defn encode-char [[a b]]
  ((row b) (column a)))

(defn decode-char [[a b]]
  (let [first-key (comp key first)
        vec-val (comp vec val)]
    (first-key
      (filter #(#{b} ((vec-val %) (column a))) chart))))

(defn cipher [f word message]
  (let [kword (format-keyword word message)
        pairs (map vector kword message)]
    (apply str (map f pairs))))

(defn decipher-char [[a b]]
  (let [index (.indexOf (row b) a)]
    (alphabet index)))

(defn decipher [encrypted original]
  (let [pairs (map vector encrypted original)
        repeating-key (map decipher-char pairs)]
    (loop [acc (vec (take 2 repeating-key))
           keyw (rest (rest repeating-key))]
      (if (= (take 2 acc) (take 2 keyw))
        (apply str acc)
        (recur (conj acc (first keyw)) (rest keyw))))))

(def encode (partial cipher encode-char))
(def decode (partial cipher decode-char))
