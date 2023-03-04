(ns aoc2022.06 
  (:require [clojure.string :as string]))

(def demo-input ["bvwbjplbgvbhsrlpgdmjqwftvncz",
                 "nppdvjthqldpwncqszvftbrmjlhg",
                 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
                 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])

(defn unique? [m] 
  (println (some "b"m))
  (not-any? (set m) m))

(defn find-four-uniques [marker? nextc]
  (if (= (count marker?) 4)
    (if (unique? (map second marker?)) 
      (reduced marker?)
      (vec (rest (conj marker? nextc))))
    (conj marker? nextc)))

(inc (first (last (reduce find-four-uniques [] (map-indexed list (demo-input 0))))))
