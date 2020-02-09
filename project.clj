(defproject euler-problems "0.1.0-SNAPSHOT"
  :description "Z's solutions to Project Euler problems."
  :license {:name "The MIT License (MIT)"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.6"]]
  :repl-options {:init-ns utils.primes})
