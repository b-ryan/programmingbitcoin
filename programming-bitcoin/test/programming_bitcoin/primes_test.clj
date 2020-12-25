(ns programming-bitcoin.primes-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
            [programming-bitcoin.primes :as p]))

(deftest primality
  (doseq [x (range (inc (apply max p/first-primes)))]
    (is (= (p/prime? x) (contains? p/first-primes x)))))
