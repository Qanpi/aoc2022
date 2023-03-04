(ns aoc2022.09
  (:require [clojure.string :as string]))

(def demo-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def directions {"R" '[ 1 0 ] "D" '[ 0 1 ] "L" '[ -1 0 ] "U" '[ 0 -1 ]})

(defn move-by [coords c2]
  (mapv + coords c2))

(defn move [coords d]
  (move-by coords (get directions d)))

(defn find-move [head tail]
  (let [normalize (fn [t x]
                    (cond 
                     (< x (- t)) (inc x) 
                     (> x t) (dec x)
                     :else x))]
    (let [diff (map - head tail)]
      (if (> (reduce + (map abs diff)) 2) 
        (mapv (partial normalize 1) diff) ;;long diagonal
        (mapv (partial normalize 0) diff))))) 

(defn execute [state instruction]
  (let [[d n] instruction
        execute-once (fn [rope] 
                       (let [heads (first rope)
                             propagate (fn [snake tails]
                                         (let [head (first (peek snake))
                                               tail (first tails)
                                               tail-move (find-move head tail)
                                               ntail (move-by tail tail-move)]
                                           (conj snake (conj tails ntail))))]
                        (reduce propagate (vector (conj heads (move (first heads) d))) (next rope))))]
    (nth (iterate execute-once state) n)))

(defn parse [input]
  (map #(update (string/split % #" ") 1 read-string) (string/split input #"\n")))


;;(execute '[([0 0]) ([0 0])] '("R" 1))

(->> (peek (reduce execute (vec (repeat 10 '([0 0]))) (parse (aoc2022.core/input "day9.txt"))))
     (into #{})
     (count))


;; (:h (:x x, :y y) :t (((:x x1 :y y1) (:x x2 :y y2) ...)

