(ns problems.s-0018)

;; Find the maximum total from top to bottom
;; (of a triangle of numbers).
;;
;; This is a dynamic programming problem.
;; For any number, the highest sum of numbers above it is easy to compute.
;; It's the below that is tricker.

;; I've done a few dynamic programming problems before; so for me,
;; the main trick was finding a data structure in which it is easy
;; to determine the parent nodes.
;;
;; I actually went down a kinda silly road at first:
;; I had started off with a single vector of numbers, then mapped over them
;; with a function that used the index (and an atom to track state) to convert
;; each entry into a vector with the value in the first position, and a vector
;; of indexes of children. 
;; Not only was that backards, but it was overly complex.
;;
;; So I set it aside, did something else, and a few hours later I realized that
;; 1) having a 'row' abstraction is necessary anyway
;; 2) it's trivial to find the indexes of the things above you in the row.
;;    (it's just that same index, and possibly the index +1).
;;
;; Clojure's `get` operator is very convenient here; not only does it avoid
;; throwing exceptions if you attempt to access an index that is out of bounds,
;; but it also lets you provide a default value. Zero seemed good,
;; though, funnily enough, any negative number would also have worked.
;;
;; Came back the next day, converted the triangle to a vector of vectors,
;; and the problem pretty much solved itself.
;;
;; You can march through the rows from the top to the bottom.
;; For each item in a row, add your greatest parent to yourself.
;; After that, there is no need to keep track of that parent row,
;; and the row becomes the new parent.
;; When you reach the final row, you can just take the max of that row
;; and call it a day.
;;
;; (When I say 'top', I mean the pointy end of the triangle.
;;  Dynamic programming has top-down and bottom up solutions;
;;  this is technically the bottom-up way to solve the problem.)

(def triangle
  [[75]
   [95 64]
   [17 47 82]
   [18 35 87 10]
   [20 04 82 47 65]
   [19 01 23 75 03 34]
   [88 02 77 73 07 63 67]
   [99 65 04 28 06 16 70 92]
   [41 41 26 56 83 40 80 70 33]
   [41 48 72 33 47 32 37 16 94 29]
   [53 71 44 65 25 43 91 52 97 51 14]
   [70 11 33 28 77 73 17 78 39 68 17 57]
   [91 71 52 38 17 14 91 43 58 50 27 29 48]
   [63 66 04 68 89 53 67 30 73 16 69 87 40 31]
   [04 62 98 27 23  9 70 98 73 93 38 53 60 04 23]])

(defn- find-max-parent-sum
  [prev-row idx itm]
  (+ itm
     (max (get prev-row idx 0)
          (get prev-row (dec idx) 0))))

(defn s-0018 [triangle-rows]
  (->> triangle-rows
       (reduce (fn [prev-row cur-row]
                 (->> cur-row
                      (map-indexed (partial find-max-parent-sum prev-row))
                      vec)))
       (reduce max)))

#_(s-0018 triangle)
