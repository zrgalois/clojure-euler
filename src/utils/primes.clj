(ns utils.primes
  (:require [clojure.math.numeric-tower :as math]))
;; Fastest approach is pulling in a large list of precalculated primes,
;;  rather than recalculating them each time.
;; But that's no fun.

;; Erasothenes
(defn trial-division-succeeds? [n primes]
  (let [ub (math/expt n 1/2)]
    (->> primes (take-while (partial >= ub)) (some #(zero? (mod n %))))))

;; To be fair, will have both algorithms skip even numnbers:
(defn primes-erasothenes
  ([] (cons 2 (primes-erasothenes 3 [2])))
  ([n primes]
   (if (trial-division-succeeds? n primes)
     (primes-erasothenes (+ n 2) primes)
     (lazy-seq (cons n (primes-erasothenes (+ n 2) (conj primes n)))))))

;; Custom algorithm;
;;  along with the growing list of primes, it keeps a sorted set
;;  of composites to strike off candidates without resorting to division.
;; Fun to write, but its more complicated, and doesn't run as fast.
;; There are probably some opportunities to increase performance further.
;; (Better underlying data structure; avoiding overlap in composites.)
(defn- primes-zach
  ([] (cons 2 (primes-zach 3 (atom (sorted-set [9 3])))))
  ([n composites]
   (if (= :prime
          (some (fn [[c p :as cp]]
                  (if (= n c) ; Equals known composite --> composite.
                    (do (doto composites  ; Update that known composite,
                          (swap! disj cp) ; since it will never trigger again.
                          (swap! conj [(+ c p p) p]))
                        :composite)
                    (if (< n c) ; Less than composite -> less than all -> prime
                      (do (swap! composites conj [(* n n) n])
                          :prime)
                      ;; If we are greater than a composite, then it will never
                      ;; trigger again and should be updated.
                      (do (doto composites
                            (swap! disj cp)
                            (swap! conj [(+ c p p) p]))
                          nil))))
                @composites))
     ;; N cannot be greater than all of our composites and also be prime.
     ;; (Which is required to walk through the some without early termination.)
     ;; By construction, our composites list includes an entry that is at least
     ;; twice that of the previously found prime; Bertrand's hypothesis will
     ;; guarantee that we find another prime in the interval between Pi and 2Pi.
     ;; Before N can increase beyond (Pi)^2, we will have found another prime.
     (lazy-seq (cons n (primes-zach (+ n 2) composites)))
     (primes-zach (+ n 2) composites))))

;; Euler algorithm:
;; (probably poorly implemented; this blows its stack easily.)
;; (there might be a way to make this into a lazy sequence like
;;  the others. That will be a challenge for later.
(defn- primes-euler [max-checked]
  (loop [primes []
         candidates (range 2 max-checked)]
    (if (seq candidates)
      (let [[p & r] candidates]
        (recur (conj primes p)
               (remove (partial
                         (into #{}
                               (eduction
                                 (comp
                                   (map (partial * p))
                                   (take-while (partial > max-checked)))
                                 candidates)))
                       r)))
      primes)))

;;;;; ;;; ;; ; ;  ;   ;     ;
;; Superficial benchmarking ;
;;;;; ;;; ;; ; ;  ;   ;     ;
#_(let [n 1753
        _ (print "eras:") p1s (time (doall (take n (primes-erasothenes))))
        _ (print "zach:") p2s (time (doall (take n (primes-zach))))
        last-prime 14983
        _ (print "eule:") p3s (time (doall (primes-euler (inc last-prime))))]
    (time (every? true? (map = p1s p2s p3s))))

#_(let [n 10000
      _ (print "eras:") p1s (time (doall (take n (primes-erasothenes))))
      _ (print "zach:") p2s (time (doall (take n (primes-zach))))]
  (time (every? true? (map = p1s p2s))))

#_(let [n 100000 ; One hundred thousand
      _ (print "eras:") p1s (time (doall (take n (primes-erasothenes))))
      _ (print "zach:") p2s (time (doall (take n (primes-zach))))]
  (time (every? true? (map = p1s p2s))))
