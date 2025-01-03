(ns aoc2022.day04.solution
  (:require [clojure.string :as str]))

(def sample-input "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn solve1 [input]
  (->> input
       (str/split-lines)
       (map #(re-seq #"\d+" %))
       (map #(map parse-long %))
       (map #(partition 2 %))
       (filter (fn [[[a-from a-to] [b-from b-to]]]
                 (or (and (<= a-from b-from) (>= a-to b-to))
                     (and (<= b-from a-from) (>= b-to a-to)))))
       count))

(solve1 sample-input)
(solve1 (slurp "resources/day04/input.txt"))

