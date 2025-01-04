(ns aoc2022.day07.solution
  (:require [clojure.string :as str]))

(def sample-input "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
")

(defn parse-input [input]
  (->> input
       (str/split-lines)))

(defn parse-command [state command]
  (cond
    (= "$ cd /" command) (assoc state :current-dir [])
    (= "$ cd .." command) (assoc state :current-dir (if (butlast (:current-dir state))
                                                      (vec (drop-last (:current-dir state)))
                                                      []))
    (= "$ ls" command) state
    (.startsWith command "$ cd") (let [dir (subs command 5)]
                                   (assoc state :current-dir (conj (:current-dir state) dir)))
    (.startsWith command "dir") (let [dir (subs command 4)
                                      current-dir (:current-dir state)
                                      new-dir (conj current-dir dir)]
                                  (assoc-in state new-dir (or (get-in state new-dir) {})))
    (re-matches #"\d+ .*" command) (let [[_ size name] (re-find #"(?:(\d+) (.*))" command)
                                         current-dir (:current-dir state)]
                                     (assoc-in state (conj current-dir name) (parse-long size)))))

(defn construct-dir [input]
  (dissoc (reduce parse-command {:current-dir []} (parse-input input)) :current-dir))

(comment
  (parse-command {:current-dir []} "304 a.doc")
  (parse-command {:current-dir []} "$ cd a")
  (parse-command {:current-dir ["b"]} "$ cd a")
  (parse-command {:current-dir ["b"]} "$ cd ..")

  (parse-input sample-input)
  (construct-dir sample-input)
  (drop-last ["a" "b" "c"])
  (butlast ["a" "b" "c"])
  (parse-command {:current-dir []} "dir b")
  (parse-command {:current-dir ["dir1"] "b.pdf" 123} "304 a.doc"))

(defn calculate-size [dir]
  (let [[size results]
        (reduce (fn [[acc results] [_ value]]
                  (if (map? value)
                    (let [[child-size child-results] (calculate-size value)]
                      [(+ acc child-size) (concat results child-results)])
                    [(+ acc value) results]))
                [0 []]
                dir)]
    [size (if (< size 100000) (conj results size) results)]))

(calculate-size (construct-dir sample-input))

(defn solve1 [input]
  (let [[_ results] (calculate-size (construct-dir input))]
    (apply + results)))

(solve1 sample-input)
(solve1 (slurp "resources/day07/input.txt"))

;; part 2

(defn calculate-size-with-each-dir [dir]
  (let [[size results]
        (reduce (fn [[acc results] [_ value]]
                  (if (map? value)
                    (let [[child-size child-results] (calculate-size value)]
                      [(+ acc child-size) (concat results child-results)])
                    [(+ acc value) results]))
                [0 []]
                dir)]
    [size (conj results size)]))

(calculate-size-with-each-dir (construct-dir sample-input))

(defn solve2 [input]
  (let [[total-size dir-sizes] (calculate-size-with-each-dir (construct-dir input))
        size-left (- 70000000 total-size)]
    (->> dir-sizes
         (sort)
         (filter #(>= (+ size-left %) 30000000))
         (first))))

(solve2 sample-input)
(solve2 (slurp "resources/day07/input.txt"))

