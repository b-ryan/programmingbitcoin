(ns programming-bitcoin.primes
  (:require [clojure.spec.alpha :as s]
            clojure.java.io
            clojure.string))

(defn prime?
  [n]
  (let [is-factor? (comp zero? (partial mod n))
        square #(* % %)]
    (cond (<= n 3) (> n 1)
          (or (is-factor? 2) (is-factor? 3)) false
          :else (->> (iterate (partial + 6) 5)
                     (map #(cond (> (square %) n) true
                                 (or (is-factor? %) (is-factor? (+ % 2)))
                                 false))
                     (drop-while nil?)
                     first))))

(def first-primes
  (->> (-> "primes.txt"
           clojure.java.io/resource
           slurp
           clojure.string/split-lines)
       (map #(Integer/parseInt %))
       (set)))

(s/def ::prime
  (s/with-gen (s/and integer?
                     pos?
                     prime?)
              #(s/gen first-primes)))
