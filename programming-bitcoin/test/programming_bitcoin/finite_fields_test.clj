(ns programming-bitcoin.finite-fields-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest testing are is use-fixtures]]
            [programming-bitcoin.finite-fields :as ff]))

(def primes (->> (-> "primes.txt" clojure.java.io/resource slurp clojure.string/split-lines)
                 (map #(Integer/parseInt %))
                 (set)))

(deftest primality
  (doseq [x (range (inc (apply max primes)))]
    (is (= (ff/prime? x) (contains? primes x)))))

(deftest finite-field-validation
  (are [number prime]
       (s/valid? ::ff/field-element (ff/->FieldElement number prime))
       1 7)
  (are [number prime]
       (not (s/valid? ::ff/field-element (ff/->FieldElement number prime)))
       1.1 7
       1 8))

(deftest equality
  (is (= (ff/->FieldElement 1 7) (ff/->FieldElement 1 7))))
