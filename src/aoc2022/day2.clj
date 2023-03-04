(ns aoc2022.02
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io]))

(def demo-input "A Y\n B X\n C Z")

(defn line-input [filename]
  (string/split (slurp (io/resource filename)) #"\n"))

(def input-map {"A" :rock, "X" :rock, "B" :paper, "Y" :paper, "C" :scissors, "Z" :scissors})
(def outcome-map {"X" :loss, "Y" :draw, "Z" :win})

(def points {:rock 1, :paper 2, :scissors 3})
(def rules {:rock :scissors, :paper :rock, :scissors :paper})
(def inverse-rules (clojure.set/map-invert rules))

(defn beats? [p1, p2]
  (= (rules p1) p2))

(defn count-points [p1, p2]
  (def outcome-score 
    (cond 
      (= p1 p2) 3
      (beats? p1 p2) 6
      :else 0)) 
  (+ outcome-score (points p1)))

(defn find-move [p2, target]
  (condp = target
    :draw p2
    :loss (rules p2)
    :win (inverse-rules p2)))

;; Part 1
(->> (for [line (line-input "day2.txt")]
      (let [inputs (string/split line #" ")]
        (def opp (input-map (inputs 0)))
        (def me (input-map (inputs 1)))

        (count-points me opp)))
  (reduce +))

;; Part 2
(->> (for [line (line-input "day2.txt")]
      (let [inputs (string/split line #" ")]
        (def opp (input-map (inputs 0)))
        (def target (outcome-map (inputs 1)))
        (def me (find-move opp target))

        (count-points me opp)))
(reduce +))

