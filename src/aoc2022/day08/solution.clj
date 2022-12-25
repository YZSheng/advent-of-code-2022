(ns aoc2022.day08.solution
  (:require [clojure.string :as string]))

(defn get-in-grid [grid [x y]]
  (nth (nth grid y) x))

(defn get-neighbours [grid x y]
  (let [width (count (first grid))
        height (count grid)]
    [(for [x (reverse (range x))]
       [x y])
     (for [x (range (inc x) width)]
       [x y])
     (for [y (reverse (range y))]
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

(def sample-grid (parse-input "resources/day08/sample.txt"))
sample-grid

(defn count-visible [grid x y]
  (let [value (get-in-grid grid [x y])
        neighour-list (get-neighbours grid x y)
        n-vals (map (fn [ls]
                      (map #(get-in-grid grid %) ls)) neighour-list)]
    (map (fn [vals] (let [result (take-while #(> value %) vals)]
                      (if (= (count result) (count vals))
                        result
                        (conj result value))
                      n-vals)))))

(defn part-two-all-products [input]
  (let [grid (parse-input input)
        w (count (first grid))
        h (count grid)]
    (for [y (range 1 (dec h))
          x (range 1 (dec w))]
      (let [visible-count (count-visible grid x y)]
        (apply * (map count visible-count))))))

(defn part-two-solution [input]
  (apply max (part-two-all-products input)))

(part-two-solution "resources/day08/sample.txt")
(part-two-solution "resources/day08/part-one.txt")

(comment

  (parse-input "resources/day08/sample.txt")
  (def sample-grid (parse-input "resources/day08/sample.txt"))
  sample-grid

  (count-visible sample-grid 2 1)
  (map (fn [ls] (map #(get-in-grid sample-grid %) ls)) (count-visible sample-grid 2 1))
  (get-in-grid sample-grid [2 1])
  (get-neighbours sample-grid 2 1)
  (map (fn [ls]
         (map #(get-in-grid sample-grid %) ls)) (get-neighbours sample-grid 2 1))

  "hello world")



