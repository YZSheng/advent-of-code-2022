(ns aoc2022.day06.solution)

(defn solve1 [input]
  (let [parsed (partition 4 1 input)]
    (loop [i 0
           inputs parsed]
      (let [input (first inputs)]
        (if (= 4 (count (set input)))
          (+ 4 i)
          (recur (inc i) (rest inputs)))))))

(solve1 "bvwbjplbgvbhsrlpgdmjqwftvncz")
(solve1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
(solve1 (slurp "resources/day06/input.txt"))

;; part 2

(defn solve2 [input]
  (let [parsed (partition 14 1 input)]
    (loop [i 0
           inputs parsed]
      (let [input (first inputs)]
        (if (= 14 (count (set input)))
          (+ 14 i)
          (recur (inc i) (rest inputs)))))))

(solve2 "bvwbjplbgvbhsrlpgdmjqwftvncz")
(solve2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
(solve2 (slurp "resources/day06/input.txt"))
