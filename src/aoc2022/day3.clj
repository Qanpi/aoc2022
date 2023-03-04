(ns aoc2022.03
  (:require [clojure.string :as string])
  (:require [clojure.set :as cljset]))

(def demo-input 
"vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defn find-shared-type [rucksack]
  (def half (/ (count rucksack) 2))
  (def comp1 (subs rucksack 0 half))
  (def comp2 (subs rucksack half (count rucksack)))
  (first (cljset/intersection (set comp1) (set comp2))))

(defn get-priority [item]
  (def ascii (int item))
  (cond 
    (<= ascii 90) (+ (- ascii 65) 27)
    :else (+ (- ascii 97) 1)))

;;Part 1
(->> (for [line (string/split (aoc2022.core/input "day3.txt") #"\n")] 
      (get-priority (find-shared-type line)))
 (reduce +))

;;Part 2
(defn find-badge [r1, r2, r3] 
  (first (cljset/intersection (set r1) (set r2) (set r3)))) 

(def input (string/split (aoc2022.core/input "day3.txt") #"\n"))

(->> (for [i (range 0 (count input) 3)] 
  (get-priority (find-badge (input i) (input (+ i 1)) (input (+ i 2)))))
(reduce +))

