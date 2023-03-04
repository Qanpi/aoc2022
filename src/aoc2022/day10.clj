(ns aoc2022.10
  (:require [clojure.string :as string]))

(def demo-input "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(defn execute [register instruction]
  (let [[cmd v] instruction
        previous (peek register)]
    (condp = cmd
      "noop" (conj register previous)
      "addx" (conj register previous (+ previous v)))))

(defn parse [input]
  (let [to-int #(when-not (nil? %) (read-string %))]
    (map #(update (string/split % #" ") 1 to-int) (string/split input #"\n"))))

(defn gen-register [input]
  (reduce execute [1] (parse input)))

;;PART 1
(->> (gen-register (aoc2022.core/input "day10.txt"))
  (drop 19)
  (take-nth 40)
  (map * (iterate (partial + 40) 20))
  (reduce +))

(defn get-pixel [i x]
  (if (and (<= (mod i 40) (inc x))
           (>= (mod i 40) (dec x)))
    "#"
    "."))

;;PART 2
(dorun (->> (gen-register (aoc2022.core/input "day10.txt"))
     (map #(get-pixel %1 %2) (iterate inc 0))
     (partition 40)
     (map println)))

