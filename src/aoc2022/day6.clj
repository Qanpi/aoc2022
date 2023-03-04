(ns aoc2022.06 
  (:require [clojure.string :as string]))

(def LENGTH 14)

(def demo-input ["bvwbjplbgvbhsrlpgdmjqwftvncz",
                 "nppdvjthqldpwncqszvftbrmjlhg",
                 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
                 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])

(defn unique? [m] 
  (= (count (distinct m)) LENGTH))

(defn find-four-uniques [marker? nextc]
  (if (= (count marker?) LENGTH)
    (if (unique? (map second marker?)) 
      (reduced marker?)
      (vec (rest (conj marker? nextc))))
    (conj marker? nextc)))

(inc (first (last (reduce find-four-uniques [] (map-indexed list (aoc2022.core/input "day6.txt"))))))
