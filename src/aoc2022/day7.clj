(ns aoc2022.07
  (:require [clojure.string :as string])
  (:require [clojure.walk :as w]))

(def demo-input "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn parse-cwd [cwd p] 
  (condp = p
    "/" (vector "/") 
    ".." (pop cwd)
    (conj cwd (str p \/))))

(defn parse-items [items]
  (->> 
    (for [item items]
      (let [[size?, tag] (string/split item #" ")]
        (condp = size?
          "dir" (vector (str tag \/) {})
          (vector tag (read-string size?)))))
    (into {})))

(defn build-tree [tree block]
  (let [lines (string/split block #"\n")
        [ cmd, p ] (string/split (first lines) #" ")]
    (condp = cmd
      "cd" (assoc tree :cwd (parse-cwd (tree :cwd) p))
      "ls" (assoc-in tree (tree :cwd) (parse-items (rest lines))))
    ))

;; cd -> navigate to folder
;; ls -> create map for all items in curr dir 
;; have a pointer to indicate which dir currently in?

;; BELOW IS MY FIRST ATTEMPT AT PART 1 - NOT VERY GOOD
;;(defn part1 [x] 
;;   (if (map? x) ;; below is ugly af but idk how to fix it
;;     (let [sizes (map second x)
;;           cum (apply + (map second (filter list? sizes))) ;; cumulative sum of all l_dir < MAX_L
;;           subdirsum (apply + (map first (filter list? sizes)))
;;           dirsum (apply + (filter number? sizes))
;;           totalsum (+ subdirsum dirsum)] ;; total sum of fully expanded dir
;;      (if (<= totalsum MAX_L)
;;              (list totalsum (+ cum totalsum))
;;              (list totalsum cum)))
;;     x))

(def MAX_L 100000)

(defn part1 [x]
  (reduce + (filter #(< % MAX_L) x)))

(def DISK_SPACE 70000000) 
(def NEEDED_SPACE 30000000)

(defn part2 [x]
  (let [sorted (sort x)
        unused (- DISK_SPACE (last sorted))
        key_ (- NEEDED_SPACE unused)
        index (java.util.Collections/binarySearch sorted key_)]
    (nth sorted (-' (inc index)))))

(->> (get (reduce build-tree {} (rest (string/split (aoc2022.core/input "day7.txt") #"\$ "))) "/")
   (w/postwalk (fn [x] 
                 (if (map? x) 
                  (let [sizes (map second x)
                        items (filter number? sizes)
                        subitems (filter seq? sizes)
                        newsum (+ (apply + (map first subitems)) (apply + items))]
                    (conj subitems newsum))
                  x)))
   (flatten)
   (part2))  ; nil
; nil
; nil

