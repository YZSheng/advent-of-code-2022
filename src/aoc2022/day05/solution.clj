(ns aoc2022.day05.solution
  (:require [clojure.string :as str]))

(def sample-input "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn transpose [matrix]
  (apply map vector matrix))

(defn parse-stacks [stacks]
  (->> stacks
       str/split-lines
       drop-last
       (map #(partition 3 4 %))
       (map #(map second %))
       transpose
       (mapv (fn [chars] (filter #(not= \space %) chars)))))

(defn parse-instructions [instructions]
  (->> instructions
       (str/split-lines)
       (map #(re-seq #"\d+" %))
       (mapv #(map parse-long %))))

(defn parse-input [input]
  (->> input
       (#(str/split % #"\n\n"))
       ((fn [[stacks instructions]]
          [(parse-stacks stacks)
           (parse-instructions instructions)]))))

(defn move
  ([stacks [amount from to] f]
   (let [from-index (dec from)
         to-index (dec to)
         moved (take amount (nth stacks from-index))
         updated-from (drop amount (nth stacks from-index))
         updated-to (concat (f moved) (nth stacks to-index))]
     (-> stacks
         (assoc from-index updated-from)
         (assoc to-index updated-to))))

  ([stacks instruction]
   (move stacks instruction identity)))

(defn solve1 [input]
  (let [[stacks instructions] (parse-input input)]
    (loop [stacks stacks
           instructions instructions]
      (if (empty? instructions)
        (apply str (map first stacks))
        (recur (move stacks (first instructions) reverse) (rest instructions))))))

(comment
  (parse-input sample-input))

(solve1 sample-input)
(solve1 (slurp "resources/day05/input.txt"))

;; part 2

(defn solve2 [input]
  (let [[stacks instructions] (parse-input input)]
    (loop [stacks stacks
           instructions instructions]
      (if (empty? instructions)
        (apply str (map first stacks))
        (recur (move stacks (first instructions)) (rest instructions))))))

(solve2 sample-input)
(solve2 (slurp "resources/day05/input.txt"))

