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

(defn solve-part-two [input]
  (->> input
       slurp
       (#(string/split % #"\n\n"))
       (map string/split-lines)
       (map (fn [nums] (map #(Integer/parseInt %) nums)))
       (map #(reduce + %))
       (sort >)
       (take 3)
       (#(reduce + %))))

(solve-part-one "resources/day01/part-one.txt")

(solve-part-two "resources/day01/sample.txt")
(solve-part-two "resources/day01/part-one.txt")
