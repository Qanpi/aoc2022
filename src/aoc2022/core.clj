(ns aoc2022.core
  (:require [clojure.java.io :as io]))

(defn input [filename]
  (slurp (io/resource filename)))
