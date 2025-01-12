(ns aoc2022.day09.solution
  (:require [clojure.string :as str]))

(def sample-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defn move-tail [head-pos tail-pos]
  (let [[head-x head-y] head-pos
        [tail-x tail-y] tail-pos]
    (cond 
      (and (= head-x tail-x) (= (- head-y 2) tail-y)) [tail-x (dec head-y)]
      (and (= head-x tail-x) (= (+ head-y 2) tail-y)) [tail-x (inc head-y)]
      (and (= head-y tail-y) (= (- head-x 2) tail-x)) [(dec head-x) tail-y]
      (and (= head-y tail-y) (= (+ head-x 2) tail-x)) [(inc head-x) tail-y]
      (and (= (dec head-x) tail-x) (= (- head-y 2) tail-y)) [head-x (dec head-y)]
      (and (= (inc head-x) tail-x) (= (- head-y 2) tail-y)) [head-x (dec head-y)]
      (and (= (dec head-x) tail-x) (= (+ head-y 2) tail-y)) [head-x (inc head-y)]
      (and (= (inc head-x) tail-x) (= (+ head-y 2) tail-y)) [head-x (inc head-y)]
      (and (= (dec head-y) tail-y) (= (- head-x 2) tail-x)) [(dec head-x) head-y]
      (and (= (inc head-y) tail-y) (= (- head-x 2) tail-x)) [(dec head-x) head-y]
      (and (= (dec head-y) tail-y) (= (+ head-x 2) tail-x)) [(inc head-x) head-y]
      (and (= (inc head-y) tail-y) (= (+ head-x 2) tail-x)) [(inc head-x) head-y]
      :else [tail-x tail-y]
      )))

(defn process-command [dir [x y]]
  (case dir
    "R" [(inc x) y]
    "L" [(dec x) y]
    "U" [x (inc y)]
    "D" [x (dec y)]))

(defn parse-input [input]
  (->> input 
       (str/split-lines)
       (map (fn [line] (str/split line #" ")))
       (map (fn [[dir steps]] [dir (parse-long steps)]))
       (map (fn [[dir steps]] (repeat steps dir)))
       (flatten)))

(parse-input sample-input)

(defn solve-part1 [input]
  (let [commands (parse-input input)]
    (loop [head-pos [0 0]
           tail-pos [0 0]
           tail-history #{}
           commands commands]
      (if (empty? commands)
        (count tail-history)
        (let [command (first commands)
              new-head-pos (process-command command head-pos)
              new-tail-pos (move-tail new-head-pos tail-pos)]
          (recur new-head-pos new-tail-pos (conj tail-history new-tail-pos) (rest commands)))))))

(solve-part1 sample-input)
(solve-part1 (slurp "resources/day09/input.txt"))
