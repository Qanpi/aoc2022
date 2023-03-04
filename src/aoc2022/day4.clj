( ns aoc2022.04 
  (:require [clojure.string :as string]))

(def demo-input "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn contains-fully? [r1, r2]
  (cond 
    (< (r1 :min) (r2 :min)) (<= (r2 :max) (r1 :max)) 
    (< (r2 :min) (r1 :min)) (<= (r1 :max) (r2 :max))
    :else true))

(defn overlap? [r1, r2]
  (cond 
    (<= (r2 :min) (r1 :max))
    (<= (r1 :min) (r2 :max))))

(defn parse-input [input]
  (for [line (string/split input #"\n")]
    (for [range_ (string/split line #",")]
      (let [split (map read-string (string/split range_ #"-"))]
        {:min (nth split 0) :max (nth split 1)}))))

(count (filter #(overlap? (nth % 0) (nth % 1)) (parse-input (aoc2022.core/input "day4.txt"))))
