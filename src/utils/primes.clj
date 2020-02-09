(ns utils.primes
  (:require [clojure.math.numeric-tower :as math]))

;; Decided to do my own algorithm.
;; Fun to write; interested in relative performance.
;; Basic premise is as we build up the list of primes,
;;  keep a tab on the next multiple of each prime.
;; If the number equals one of those, we can go ahead and
;;  scratch it off the list.
;; If it is greater than a multiple, we advance that multiple to the next.
;; If it is less than a multiple, we know that it is prime, because
;;  our multiples are sorted from least to greatest.
;; (Keeping it in a sorted set is an attempt to increase performance;
;;  it gives us the ability to do early exits.)
;; [If I really wanted to increase performance, I'd probably use a mutable
;;  sorted collection directly, rather than wrapping a persistent tree set
;;  in an atom.]
;; You'll also see a few small optimizations, such as taking 2 as a base case,
;; not checking even numbers, and avoiding even numbers in our composites list.
(defn primes-zach
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

;; Now for Erasothenes:
(defn trial-division-succeeds? [n primes]
  (let [ub (math/expt n 1/2)]
    (->> primes (some #(zero? (mod n %))))))

;; To be fair, will have both algorithms skip even numnbers:
(defn- primes-erasothenes
  ([] (cons 2 (primes-erasothenes 3 [2])))
  ([n primes]
   (if (trial-division-succeeds? n primes)
     (primes-erasothenes (+ n 2) primes)
     (lazy-seq (cons n (primes-erasothenes (+ n 2) (conj primes n)))))))

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

;; Euler's is fastest for small numbers:
#_(let [n 1753
        _ (print "eras:") p1s (time (doall (take n (primes-erasothenes))))
        _ (print "zach:") p2s (time (doall (take n (primes-zach))))
        last-prime 14983
        _ (print "eule:") p3s (time (doall (primes-euler (inc last-prime))))]
    (time (every? true? (map = p1s p2s p3s))))
;; But, it quickly reaches stack overflows for larger ones:
#_(primes-euler 15000)
#_(primes-euler 100000)
;; (Remember that this isn't asking for the 100000th prime;
;;  it is asking that the first 100000 numbers be filtered.)

;; My algorithm prevails with larger ones:
#_(let [n 10000
      _ (print "eras:") p1s (time (doall (take n (primes-erasothenes))))
      _ (print "zach:") p2s (time (doall (take n (primes-zach))))]
  (time (every? true? (map = p1s p2s))))

;; (This one takes some time;
;;  Erasothenes ~ 11.66 minutes
;;  Mine        ~ 00.24 minutes)
#_(let [n 100000 ; One hundred thousand
      _ (print "eras:") p1s (time (doall (take n (primes-erasothenes))))
      _ (print "zach:") p2s (time (doall (take n (primes-zach))))]
  (time (every? true? (map = p1s p2s))))

;; The million dollar question:
#_(time (doall (take 1000000 (primes-zach))))
;; Takes ~ 3.76 minutes to run
;; 15,485,863 is the 1,000,000th prime.
