(ns problems.0001)
;; Find the sum of all the multiples of 3 or 5 below 1000

;; Naive solution, linear time:
#_(defn s-0001 [upr-bnd-excl]
    (transduce (filter (fn [n] (or (zero? (mod n 3))
                                   (zero? (mod n 5)))))
               +
               (range upr-bnd-excl)))

;; Constant time solution:
(defn s-0001 [upr-bnd-excl]
  (let [upr-bnd-incl (dec upr-bnd-excl)
        ub-3  (quot upr-bnd-incl 3)
        ub-5  (quot upr-bnd-incl 5)
        ub-15 (quot upr-bnd-incl 15)]
  (- (+ (* 3/2 ub-3 (inc ub-3))
        (* 5/2 ub-5 (inc ub-5)))
     (* 15/2 ub-15 (inc ub-15)))))

#_(time (s-0001 1000))
#_(time (s-0001 100000000))
#_(time (s-0001 10000000000000000000000000000))

;; Premise:
;; Use formula: 1 + 2 + ... + n = n(n+1)/2
;; Add multiples of 3 and multiples of 5.
;; Subtract out the overlap (multiples of 15).

;; Next challenge would be doing this for arbitrary divisors.
