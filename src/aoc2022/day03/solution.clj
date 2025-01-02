(ns aoc2022.day03.solution
  (:require [clojure.string :as str]
            [clojure.set :refer [intersection]]))

(def sample-input "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defn split-str [input]
  (let [half (quot (count input) 2)]
    [(subs input 0 half) (subs input half)]))

(defn find-common-char [colls]
  (first (reduce intersection (map set colls))))

(defn priority [c]
  (let [intcode (int c)]
    (if (< 90 intcode)
      (- intcode 96)
      (- intcode 38))))

(defn solve1 [input]
  (->> input
       (str/split-lines)
       (map split-str)
       (map find-common-char)
       (map priority)
       (reduce +)))

(solve1 sample-input)
(solve1 (slurp "resources/day03/input.txt"))

;; part 2

(defn solve2 [input]
  (->> input
       (str/split-lines)
       (partition 3)
       (map find-common-char)
       (map priority)
       (reduce +)))

(solve2 sample-input)
(solve2 (slurp "resources/day03/input.txt"))
