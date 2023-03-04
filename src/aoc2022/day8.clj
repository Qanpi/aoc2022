(ns aoc2022.08
  (:require [clojure.string :as string]))

(def demo-input "30373
25512
65332
33549
35390")

(defn read-char [c] 
  (Character/digit c 10))

(defn parse-map [input] 
  (vec (mapv #(mapv read-char (seq %)) (string/split input #"\n"))))

(defn get-maxima [line]
  (reduce #(conj %1 (apply max %2 %1)) [-1] line))

(defn part1 [line]
  (mapv > line (get-maxima line)))

(defn variadic-or [& args]
  (some true? args))

(defn r [x] (map reverse x))
(defn t [x] (apply map vector x))
(def tr (comp t r))
(def rt (comp r t))

(defn transform-map [nmap]
  ((juxt identity r t rt) nmap))

;;PART 1
(->> (transform-map (parse-map demo-input)) ;;transform the map in all 4 directions to make use of the same f
  (map (partial mapv part1))
  (map #(%1 %2) [identity r t tr]) ;;reverse functions above
  (reduce #(map (partial map variadic-or) %1 %2))

  (map (partial filter some?))
  (map count)
  (reduce +))

;;PART 2
(defn tails [a-seq] 
  (take-while seq (iterate rest a-seq)))

(defn part2 [line]
  (loop [x (first line)
         upcoming (rest line)
         sight 0]
    (if (or (empty? upcoming) 
            (<= x (first upcoming)))
      (if (empty? upcoming) sight (inc sight)) ;;if corner of map, don't increment (no tree to see)
      (recur x
             (rest upcoming)
             (inc sight)))))

(->> (transform-map (parse-map (aoc2022.core/input "day8.txt")))
  (map (partial map tails))
  (map #(map (partial map part2) %))

  (map #(%1 %2) [identity r t tr]) ;;reverse functions above

  (reduce (partial map (partial map *)))

  (map (partial reduce max))
  (reduce max))
