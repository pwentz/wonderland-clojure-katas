(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn candidates [word]
  (filter #(and (= (count %) (count word))
                (not= % word))
          words))

(defn diff-count [a b]
  (count (remove (partial apply =) (map vector a b))))

(defn are-doublets? [a b]
  (= (diff-count a b)
     1))

(defn find-doublet [start end coll]
  (->> coll
       (filter #(are-doublets? start %))
       (sort-by #(diff-count end %))
       (first)))

(defn doublets [start-word end-word]
  (loop [acc [start-word]
         poss (candidates start-word)]
    (let [last-doublet (last acc)
          next-doublet (find-doublet last-doublet end-word poss)]
      (cond (= last-doublet end-word) acc
            (nil? next-doublet) []
            :else (recur (conj acc next-doublet)
                         (remove #{last-doublet} poss))))))
