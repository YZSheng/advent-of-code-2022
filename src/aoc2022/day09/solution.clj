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
      (and (= (- head-x 2) tail-x) (= (- head-y 2) tail-y)) [(dec head-x) (dec head-y)]
      (and (= (+ head-x 2) tail-x) (= (+ head-y 2) tail-y)) [(inc head-x) (inc head-y)]
      (and (= (- head-x 2) tail-x) (= (+ head-y 2) tail-y)) [(dec head-x) (inc head-y)]
      (and (= (+ head-x 2) tail-x) (= (- head-y 2) tail-y)) [(inc head-x) (dec head-y)]
      :else [tail-x tail-y])))

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

(defn generate-chain [initial-pos length]
  (vec (repeat length initial-pos)))

(defn update-chain [dir chain]
  (let [new-head (process-command dir (first chain))]
    (reduce
     (fn [updated-chain idx]
       (let [prev-knot (nth updated-chain (dec idx))
             curr-knot (nth updated-chain idx)]
         (assoc updated-chain idx (move-tail prev-knot curr-knot))))
     (assoc chain 0 new-head)
     (range 1 (count chain)))))

(defn solve-part1 [input]
  (let [commands (parse-input input)]
    (loop [chain (generate-chain [0 0] 2)
           tail-history #{}
           commands commands]
      (if (empty? commands)
        (count tail-history)
        (let [command (first commands)
              updated-chain (update-chain command chain)
              new-tail-pos (last updated-chain)]
          (recur updated-chain (conj tail-history new-tail-pos) (rest commands)))))))

(solve-part1 sample-input)
(solve-part1 (slurp "resources/day09/input.txt"))

(def large-input "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(defn solve-part2 [input]
  (let [commands (parse-input input)]
    (loop [chain (generate-chain [0 0] 10)
           tail-history #{}
           commands commands]
      (if (empty? commands)
        (count tail-history)
        (let [command (first commands)
              updated-chain (update-chain command chain)
              new-tail-pos (last updated-chain)]
          (recur updated-chain (conj tail-history new-tail-pos) (rest commands)))))))

(solve-part2 large-input)
(solve-part2 (slurp "resources/day09/input.txt"))
