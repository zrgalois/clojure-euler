(ns problems.0003
  (:require [utils.primes :as primes])) ; where the magic truly happens.
;; What is the largest prime factor of the number 600851475143?

(defn s-0003 [n]
  (let [n-atom (atom n)]
    (some (fn [p]
            (while (zero? (mod @n-atom p))
              (swap! n-atom quot p))
            (and (<= @n-atom p) p))
          (primes/primes-erasothenes))))

#_(time (s-0003 600851475143))
#_(time (s-0003 (*' 937 937 937 937 937 600851475143)))
#_(time (s-0003 600851475143123))
;; This takes a while...
;; Apparently, I accidentally picked a number with exactly 2 prime factors.
;; 4540079 and 132343837
;; #AccidentalRSA

;; Wolfram alpha could do it in seconds though.
;; Clearly, better algorithms for prime factorization exist.
;; Next challenge would be to find one of those, or generate
;; the primes ahead of time and attempt a parallel solution.
