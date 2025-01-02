(ns aoc2022.day02.solution
  (:require [clojure.string :as str]))

(def sample-input "A Y
B X
C Z")

;; opponent
;; A - Rock
;; B - Paper
;; C - Scissors

;; yours
;; X - Rock - 1 point
;; Y - Paper - 2 points
;; Z - Scissors - 3 points

;; Round
;; win - 6 points
;; draw - 3 points
;; lose - 0 point

(defn calc-score [opponent yours]
  (case yours
    "X" (case opponent
          "A" (+ 1 3)
          "B" (+ 1 0)
          "C" (+ 1 6))
    
    "Y" (case opponent
          "A" (+ 2 6)
          "B" (+ 2 3)
          "C" (+ 2 0))
    
    "Z" (case opponent
          "A" (+ 3 0)
          "B" (+ 3 6)
          "C" (+ 3 3))
    ))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(str/split % #" "))))

(parse-input sample-input)

(defn solve1 [input]
  (->> input
       (parse-input)
       (map #(apply calc-score %))
       (reduce +)))

(solve1 sample-input)

(solve1 (slurp "resources/day02/input.txt"))

(comment 

  (calc-score "A" "Y") 
  (calc-score "B" "X") 
  (calc-score "C" "Z"))
;; opponent
;; A - Rock
;; B - Paper
;; C - Scissors

;; yours
;; Rock - 1 point
;; Paper - 2 points
;; Scissors - 3 points

;; Round
;; X to lose - 0 point 
;; Y to draw - 3 points
;; Z to win - 6 points

(defn calc-score-2 [opponent yours]
  (case yours
    "X" (case opponent
          "A" (+ 3 0)
          "B" (+ 1 0)
          "C" (+ 2 0))
    
    "Y" (case opponent
          "A" (+ 1 3)
          "B" (+ 2 3)
          "C" (+ 3 3))
    
    "Z" (case opponent
          "A" (+ 2 6)
          "B" (+ 3 6)
          "C" (+ 1 6))
    ))

(comment
  
  (calc-score-2 "A" "Y") 
  (calc-score-2 "B" "X") 
  (calc-score-2 "C" "Z"))

(defn solve2 [input] (->> input (parse-input) (map #(apply calc-score-2 %))
                          (reduce +)))

(solve2 sample-input) 
(solve2 (slurp "resources/day02/input.txt"))
