(ns alphabet-cipher.coder)

(def letter-map
  {:A 1 :B 2 :C 3 :D 4 :E 5
   :F 6 :G 7 :H 8 :I 9 :J 10
   :K 11 :L 12 :M 13 :N 14
   :O 15 :P 16 :Q 17 :R 18
   :S 19 :T 20 :U 21 :V 22
   :W 23 :X 24 :Y 25 :Z 26})

(def chart
  {:A "abcdefghijklmnopqrstuvwxyz"
   :B "bcdefghijklmnopqrstuvwxyza"
   :C "cdefghijklmnopqrstuvwxyzab"
   :D "defghijklmnopqrstuvwxyzabc"
   :E "efghijklmnopqrstuvwxyzabcd"
   :F "fghijklmnopqrstuvwxyzabcde"
   :G "ghijklmnopqrstuvwxyzabcdef"
   :H "hijklmnopqrstuvwxyzabcdefg"
   :I "ijklmnopqrstuvwxyzabcdefgh"
   :J "jklmnopqrstuvwxyzabcdefghi"
   :K "klmnopqrstuvwxyzabcdefghij"
   :L "lmnopqrstuvwxyzabcdefghijk"
   :M "mnopqrstuvwxyzabcdefghijkl"
   :N "nopqrstuvwxyzabcdefghijklm"
   :O "opqrstuvwxyzabcdefghijklmn"
   :P "pqrstuvwxyzabcdefghijklmno"
   :Q "qrstuvwxyzabcdefghijklmnop"
   :R "rstuvwxyzabcdefghijklmnopq"
   :S "stuvwxyzabcdefghijklmnopqr"
   :T "tuvwxyzabcdefghijklmnopqrs"
   :U "uvwxyzabcdefghijklmnopqrst"
   :V "vwxyzabcdefghijklmnopqrstu"
   :W "wxyzabcdefghijklmnopqrstuv"
   :X "xyzabcdefghijklmnopqrstuvw"
   :Y "yzabcdefghijklmnopqrstuvwx"
   :Z "zabcdefghijklmnopqrstuvwxy"})

(defn format-keyword [word message]
  (apply str (take (count message) (cycle word))))

(def to-keyword (comp keyword clojure.string/upper-case))

(def column (comp dec letter-map to-keyword))
(def row (comp vec chart to-keyword))

(defn assemble-encoding [[a b]]
  ((row b) (column a)))

(defn assemble-decoding [[a b]]
  (let [to-char (comp first seq clojure.string/lower-case name key first)]
    (to-char
      (filter #(#{b} ((vec (val %)) (column a))) chart))))

(defn cipher [f word message]
  (let [kword (format-keyword word message)
        enc-pairs (map vector kword message)]
    (apply str (map f enc-pairs))))

(def encode (partial cipher assemble-encoding))
(def decode (partial cipher assemble-decoding))
