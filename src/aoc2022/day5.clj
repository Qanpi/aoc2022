(ns aoc2022.05
  (:require [clojure.string :as string]))

(def demo-input "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")


(defn parse-stacks [input] 
    (->> (for [row (string/split input #"\n")]
      (map #(first (remove #{\space \[ \]} %)) (partition 3 4 row)))
     (drop-last) 
     (apply map list) ;; transpose
     (mapv #(apply list (filter some? %))))) 

(defn parse-recipe [input] 
  (for [row (string/split input #"\n")]
    (let [digits (map read-string (re-seq #"\d+" row))]
      (zipmap '(:n :from :to) digits))))

(defn part1 [s n to from]
  (loop [i 0
         s s]
    (if (< i n)
      (recur (inc i)
        (-> (update s to #(conj % (peek (get s from))))
          (update from pop)))
      s)))

(defn part2 [s n to from]
  (-> (update s to #(apply conj % (reverse (take n (get s from))))) 
      (update from #(nthrest % n))))

(defn apply-recipe [s r]
  (let [n (r :n)
        from (dec (r :from))
        to (dec (r :to))]
     (part2 s n to from)))

(let [[s r] (string/split (aoc2022.core/input "day5.txt") #"\n\n")]
    (apply str (map peek (reduce apply-recipe (parse-stacks s) (parse-recipe r)))))
