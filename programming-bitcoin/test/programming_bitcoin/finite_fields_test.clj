(ns programming-bitcoin.finite-fields-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest testing are is use-fixtures]]
            [programming-bitcoin.finite-fields :as f]))

(deftest finite-field-validation
  (are [number prime]
       (s/valid? ::f/field-element (f/e number prime))
       1
       7)
  (are [number prime]
       (not (s/valid? ::f/field-element (f/e number prime)))
       1.1 7
       1 8))

(deftest equality
  (is (= (f/e 1 7) (f/e 1 7))))

(deftest sub
  (is (= (f/e 25 31) (f/sub (f/e 29 31) (f/e 4 31))))
  (is (= (f/e 16 31) (f/sub (f/e 15 31) (f/e 30 31)))))

(deftest pow
  (is (= (f/e 15 31) (f/pow (f/e 17 31) 3)))
  (is (= (f/e 16 31)
         (f/mul (f/pow (f/e 5 31) 5)
                (f/e 18 31)))))

(deftest div
  (is (= (f/e 4 31) (f/div (f/e 3 31) (f/e 24 31))))
  (is (= (f/e 13 31) (f/mul (f/pow (f/e 4 31) -4) (f/e 11 31)))))
