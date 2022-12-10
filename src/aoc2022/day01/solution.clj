(ns aoc2022.day01.solution
  (:require [clojure.string :as string]))

(defn solve-part-one [input]
  (->> input
       slurp
       (#(string/split % #"\n\n"))
       (map string/split-lines)
       (map (fn [nums] (map #(Integer/parseInt %) nums)))
       (map #(reduce + %))
       (apply max)))

(solve-part-one "resources/day01/part-one.txt")