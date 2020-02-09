(ns problems.0002
  (:require [clojure.math.numeric-tower :as m]))
;; Sum the even fibonacci numbers <= 4000000
;; (Starting with 1 & 2, rather than 1 & 1)
;; 1, 2, 3, 5, 8, 21, 34, 55, 89, 144

;;; Naive solution
;; +' automatically promotes numerics, + doesn't
(defn fibonacci
  ([] (fibonacci 1 2))
  ([a b] (lazy-seq (cons a (fibonacci b (+' a b)))))) 

(defn s-0002 [ub]
  (transduce (comp (filter even?)
                   (take-while #(< % ub)))
             +
             (fibonacci)))

#_(time (s-0002 4000000))
#_(time (s-0002 (m/expt 1000000 100)))
#_(time (s-0002 (m/expt 1000000 200)))
#_(time (s-0002 (m/expt 1000000 400)))
#_(time (s-0002 (m/expt 1000000 800)))
#_(time (s-0002 (m/expt 1000000 1600)))
#_(time (s-0002 (m/expt 1000000 3200)))
#_(time (s-0002 (m/expt 1000000 6400)))
#_(time (s-0002 (m/expt 1000000 12800)))
;; Was planning to move past the naive solution,
;; but honestly, it's pretty darn efficient.

;; If you want to get fancy, used the closed forumla for the fibonacci numbers:
;; http://mathonline.wikidot.com/a-closed-form-of-the-fibonacci-sequence
;;
;; Then, prove that Xn = 2(Fn - 1) + F(n-1),
;; where Xn = F1 + F2 + ... + Fn
;; Once that is set, plug in the closed formula for the Fns above,
;; and voilah! You have achieved your constant time solution
;; ...
;; That is, you have achieved your constant time solution for the sum of all
;; fibonacci... only problem is, the problem asked for the sum of the even
;; ones.
;; There's probably another trick left then.
;; Every 3rd in the sequence (starting with 2) is even, and that is easily
;; proved. It might play into the equation somewhere
;; ---
;; That said, I'd rather move on to problems I haven't solved at all before
;; rehashing old ones.
