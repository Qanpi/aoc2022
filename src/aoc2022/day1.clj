(ns aoc2022.01
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io]))

(def demo-input "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defn input [filename]
  (slurp (io/resource filename)))

(defn paragraph-split [s]
  (for [paragraph (string/split s #"\n *\n *")]
    (string/split paragraph #"\n *")))

(->> (for [elf-calories (paragraph-split (input "day1.txt"))]
  (reduce + (map parse-long elf-calories))) 
(apply max))

(->> (for [elf-calories (paragraph-split (input "day1.txt"))]
  (reduce + (map parse-long elf-calories))) 
(sort-by -)
(take 3)
(reduce +))
