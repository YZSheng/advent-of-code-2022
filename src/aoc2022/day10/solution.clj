(ns aoc2022.day10.solution
  (:require [clojure.string :as str]))

(def sample-input "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
")

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (mapv #(str/split % #" "))
       (mapv (fn [[op arg]] [(keyword op) ((fnil parse-long "0") arg)]))))

(parse-input sample-input)

(defn generate-results [instructions]
  (loop [instructions instructions
         result [1]]
    (if (empty? instructions)
      result
      (let [[instruction & rest-instructions] instructions
            [op arg] instruction
            total (last result)]
        (case op
          :addx (recur rest-instructions (conj (conj result total) (+ arg total)))
          :noop (recur rest-instructions (conj result total)))))))

(defn solve1 [input]
  (let [instructions (parse-input input)
        results (generate-results instructions)]
    (->> [20 60 100 140 180 220]
         (map (fn [n] (* n (nth results (dec n)))))
         (reduce +))))

(solve1 sample-input)
(solve1 (slurp "resources/day10/input.txt"))

(defn solve2 [input]
  (let [instructions (parse-input input)
        results (generate-results instructions)]
    (doseq [i (range 240)]
      (let [sprite-location (nth results i)
            sprite-location-row (mod sprite-location 40)
            crt (mod i 40)
            should-print (and (>= crt (dec sprite-location-row)) (<= crt (inc sprite-location-row)))]
        (if should-print (print "#") (print "."))
        (if (= crt 39) (println))))))

(solve2 sample-input)
(solve2 (slurp "resources/day10/input.txt"))

