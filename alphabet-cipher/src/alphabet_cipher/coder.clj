(ns alphabet-cipher.coder)

(def key-count 26)

(def alphabet (vec (take key-count (iterate (comp char inc int) \a))))

(def cycled-alphabet
  (map
    #(vec
       (drop %
             (take (+ key-count %) (cycle alphabet))))
    (range key-count)))

(def chart
  (into {} (map vector alphabet cycled-alphabet)))

(defn format-keyword [word message]
  (apply str (take (count message) (cycle word))))

(defn column [a]
  (.indexOf alphabet a))

(def row chart)

(defn encode-char [[a b]]
  ((row b) (column a)))

(defn decode-char [[a b]]
  (let [first-key (comp key first)]
    (first-key
      (filter #(#{b} ((val %) (column a))) chart))))

(defn cipher [f word message]
  (let [kword (format-keyword word message)
        pairs (map vector kword message)]
    (apply str (map f pairs))))

(defn decipher-char [[a b]]
  (let [index (.indexOf (row b) a)]
    (alphabet index)))

(defn decipher [encrypted original]
  (let [encrypted-pairs (map vector encrypted original)
        repeating-key (apply str (map decipher-char encrypted-pairs))
        identifiers (subs repeating-key 0 2)
        next-identifiers (.indexOf (subs repeating-key 2) identifiers)
        restart-point (+ next-identifiers 2)]
    (subs repeating-key 0 restart-point)))

(def encode (partial cipher encode-char))
(def decode (partial cipher decode-char))
