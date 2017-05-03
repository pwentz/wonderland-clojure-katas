(ns alphabet-cipher.coder
  (:require [clojure.string :as st]))

(def letter-map
  {:A 0 :B 1 :C 2 :D 3 :E 4
   :F 5 :G 6 :H 7 :I 8 :J 9
   :K 10 :L 11 :M 12 :N 13
   :O 14 :P 15 :Q 16 :R 17
   :S 18 :T 19 :U 20 :V 21
   :W 22 :X 23 :Y 24 :Z 25})

(def alphabet (take 26 (iterate (comp char inc int) \a)))

(def cycled-alphabet
  (map #(apply str (drop % (take (+ 26 %) (cycle alphabet)))) (range 26)))

(def chart
  (into {} (map vector (map to-keyword alphabet) (do-stuff))))

(defn format-keyword [word message]
  (apply str (take (count message) (cycle word))))

(def to-keyword (comp keyword st/upper-case))

(def column (comp letter-map to-keyword))
(def row (comp vec chart to-keyword))

(def to-char (comp first st/lower-case name key first))

(defn assemble-encoding [[a b]]
  ((row b) (column a)))

(defn assemble-decoding [[a b]]
  (to-char
    (filter #(#{b} ((vec (val %)) (column a))) chart)))

(defn cipher [f word message]
  (let [kword (format-keyword word message)
        pairs (map vector kword message)]
    (apply str (map f pairs))))

(defn assemble-decipher [[a b]]
  (let [index (.indexOf (row b) a)]
    (to-char
      (filter #(#{index} (val %)) letter-map))))

(defn decipher [encrypted original]
  (let [pairs (map vector encrypted original)
        repeating-key (map assemble-decipher pairs)]
    (loop [acc (vec (take 2 repeating-key))
           keyw (rest (rest repeating-key))]
      (if (= (take 2 acc) (take 2 keyw))
        (apply str acc)
        (recur (conj acc (first keyw)) (rest keyw))))))

(def encode (partial cipher assemble-encoding))
(def decode (partial cipher assemble-decoding))
