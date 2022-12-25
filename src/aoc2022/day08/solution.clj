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

(get-neighbours (parse-input "resources/day08/sample.txt") 2 1)

(defn find-inners [w h pairs]
  (filter (fn [[x y]]
            (and (< 0 x)
                 (< 0 y)
                 (< x w)
                 (< y h))) pairs))

(defn find-inner-neighbours [grid neighbours]
  (let [w (count (first grid))
        h (count grid)]
    (map #(find-inners w h %) neighbours)))

(def sample-grid (parse-input "resources/day08/sample.txt"))
sample-grid

(find-inner-neighbours sample-grid (get-neighbours sample-grid 2 1))

(get-neighbours sample-grid 2 1)
(map #(find-inners 5 5 %) (get-neighbours sample-grid 2 1))
(find-inners 5 5 [[1 1]])

(defn count-visible [grid x y]
  (let [value (get-in-grid grid [x y])
        neighour-list (get-neighbours grid x y)
        inner-neighbours (find-inner-neighbours grid neighour-list)
        n-vals (map (fn [ls]
                      (map #(get-in-grid grid %) ls)) neighour-list)]
    (map (fn [vals] (let [result (take-while #(> value %) vals)]
                      (if (= (count result) (count vals))
                        result
                        (conj result value))
                      n-vals)))))

(count-visible sample-grid 2 1)
(count-visible sample-grid 2 3)

(defn part-two-solution [input]
  (let [grid (parse-input input)]
    (for [x (range (count (first grid)))
          y (range (count grid))
          visible-count (count-visible grid x y)]
      (->> visible-count
           (filter not-empty)))))

(part-two-solution "resources/day08/sample.txt")
(count-visible sample-grid 0 0)

(partition-by #(> 5 %) [1 2 3 4 5 5 4 3 5 1])

(->> [[] [1 2] [3 4]]
     (filter not-empty)
     (map count)
     (apply *))

(apply max [1 2 4 3 5])

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



