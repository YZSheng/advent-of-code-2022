(ns aoc2022.day08.solution
  (:require [clojure.string :as string]))

(defn get-in-grid [grid [x y]]
  (nth (nth grid y) x))

(defn get-neighbours [grid x y]
  (let [width (count (first grid))
        height (count grid)]
    [(for [x (range x)]
       [x y])
     (for [x (range (inc x) width)]
       [x y])
     (for [y (range y)]
       [x y])
     (for [y (range (inc y) height)]
       [x y])]))

(defn is-visible [grid x y]
  (let [value (get-in-grid grid [x y])
        neighbours (get-neighbours grid x y)]
    (some (fn [n-vals]
            (every? #(> value (get-in-grid grid %)) n-vals)) neighbours)))

(defn parse-input [input]
  (->> input
       slurp
       string/split-lines
       (map #(clojure.string/split % #""))
       (map (fn [row]
              (map #(Integer/parseInt %) row)))))

(defn part-one-solution [input]
  (let [grid (parse-input input)
        w (count (first grid))
        h (count grid)
        inner-visible  (->> (for [y (range 1 (dec h))
                                  x (range 1 (dec w))]
                              (is-visible  grid x y))
                            (filter true?)
                            count)]
    ;; four corner values are counted twice
    (+ inner-visible w h w h -4)))

(part-one-solution "resources/day08/sample.txt")
(part-one-solution "resources/day08/part-one.txt")

(comment

  (parse-input "resources/day08/sample.txt")
  (->> "resources/day08/sample.txt"
       slurp
       string/split-lines
       (map #(clojure.string/split % #""))
       (map (fn [row]
              (map #(Integer/parseInt %) row)))))
