(ns aoc2022.11
  (:require [clojure.string :as string]))

(def demo-input "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(defrecord Monkey [id items worryf divisor iftrue iffalse activity])

(defn parse-op [line]
  (let [[ _ p1 sym p2] (re-find #"new = (\w+) (\W) (\w+)" line)
        op (resolve (symbol (str sym "'")))
        constant (read-string (if (= p1 "old") p2 p1))]
    (cond 
      (= p1 p2 "old") #(reduce op (repeat 2 %))
      :else (partial op constant))))

(defn parse [input] 
  (vec (for [monkey (string/split input #"\n\n")]
    (let [lines (string/split monkey #"\n")
          parse-int #(read-string (re-find #"\d+" %))
          parse-ints #(mapv read-string (re-seq #"\d+" %))]
      (apply ->Monkey
             (conj (mapv #(%1 %2) [parse-int
                                   parse-ints
                                   parse-op
                                   parse-int
                                   parse-int
                                   parse-int] lines) 
               0)))))) ;;for activity

(defn calc-worry [f x]
  (f x)) ;;quot _ 3 for part 1

(defn test-worry [x d]
  (zero? (mod x d)))

(defn complete-round [monkeys]
  (loop [i 0
         monkeys monkeys] 
    (if (< i (count monkeys))
      (recur (inc i)
             (let [m (monkeys i)
                   add-activity #(update % :activity + (count (:items m)))
                   extract-items (fn [m] 
                                   (let [{items :items
                                          worryfunc :worryf
                                          div :divisor} m]
                                     (->> (for [item items]
                                         (let [worry (calc-worry worryfunc item) 
                                               to ((if (test-worry worry div) :iftrue :iffalse) m)]
                                       (vector to worry))))))
                   distribute-items #(update-in %1 [(first %2) :items] conj (second %2))
                   empty-items #(assoc %1 :items [])]
                (reduce distribute-items (assoc monkeys i (empty-items (add-activity m))) (extract-items m))))
      monkeys)))

(->> (nth (iterate complete-round (parse demo-input)) 20)
     (map :activity)
     )

;;(-> (update-in monkeys [to :items] conj nworry)
       ;;(update-in [i :items] (comp vec rest)))

