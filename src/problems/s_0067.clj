(ns problems.s-0067
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [problems.s-0018 :as p]))

;; Start by loading the file and converting it to a vector of vecotrs.

(def big-triangle
  (->> "p067_triangle.txt"
       io/resource
       slurp
       str/split-lines
       (map #(str/split % #"\s"))
       (mapv (partial mapv #(Integer/parseInt %)))))

#_(p/s-0018 big-triangle)
