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
  (let [half (/ (count input) 2)]
    [(subs input 0 half) (subs input half)]))

(defn find-common-char [[a b]]
  (first (intersection (set a) (set b))))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map split-str)
       (map find-common-char)))

(defn priority [c]
  (let [intcode (int c)]
    (if (< 90 intcode)
      (- intcode 96)
      (- intcode 38))))

(comment
  (set "abc")

  (find-common-char ["abc" "aeg"])
  (parse-input sample-input)

  (int \a)
  (int \A)
  (int \Z)
  (priority \a)
  (priority \z)
  (priority \A)
  (priority \Z))

(split-str "abcd")
(defn solve1 [input]
  (->> input
       parse-input
       (map priority)
       (reduce +)))

(solve1 sample-input)
(solve1 (slurp "resources/day03/input.txt"))
