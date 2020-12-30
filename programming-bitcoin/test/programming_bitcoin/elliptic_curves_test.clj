(ns programming-bitcoin.elliptic-curves-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]
            [programming-bitcoin.elliptic-curves :as ec]
            [programming-bitcoin.finite-fields :as f]))

(deftest valid-points
  (doseq [[x y valid?] [[2 4 false] [-1 -1 true] [18 77 true] [5 7 false]
                        [:inf :inf true]]]
    (is (= (s/valid? ::ec/point (ec/bi-p x y 5 7)) valid?))))

(deftest add-scalars-inf
  (let [a (ec/inf 5 7)
        b (ec/bi-p 2 5 5 7)
        c (ec/bi-p 2 -5 5 7)]
    (is (= b (ec/add a b)))
    (is (= b (ec/add b a)))))

(deftest add-scalars-x-equal
  (let [a (ec/inf 5 7)
        b (ec/bi-p 2 5 5 7)
        c (ec/bi-p 2 -5 5 7)]
    (is (= a (ec/add b c)))))

(deftest add-scalars-same-point
  (let [a (ec/bi-p -1 -1 5 7) b (ec/bi-p 18 77 5 7)] (is (= b (ec/add a a)))))

(deftest add-scalars-nothing-equal
  (let [a (ec/bi-p 3 7 5 7)
        b (ec/bi-p -1 -1 5 7)
        c (ec/bi-p 2 -5 5 7)]
    (is (= c (ec/add a b)))))

(deftest add-finite-field-elements
  (let [prime 223
        a (f/e 0 prime)
        b (f/e 7 prime)
        additions (list [192 105 17 56 170 142]
                        [47 71 117 141 60 139]
                        [143 98 76 66 47 71])]
    (doseq [[x1 y1 x2 y2 x3 y3] additions]
      (let [p1 (ec/p (f/e x1 prime) (f/e y1 prime) a b)
            p2 (ec/p (f/e x2 prime) (f/e y2 prime) a b)
            p3 (ec/p (f/e x3 prime) (f/e y3 prime) a b)]
        (is (= p3 (ec/add p1 p2)))))))

(deftest scalar-mul-points
  (let [prime 223
        a (f/e 0 prime)
        b (f/e 7 prime)
        multiplications (list [2 192 105 49 71]
                              [2 143 98 64 168]
                              [2 47 71 36 111]
                              [4 47 71 194 51]
                              [8 47 71 116 55]
                              [21 47 71 nil nil])]
    (doseq [[coeff x1-raw y1-raw x2-raw y2-raw :as row] multiplications
            :let [p1 (ec/p (f/e x1-raw prime) (f/e y1-raw prime) a b)
                  p2 (if x2-raw
                       (ec/p (f/e x2-raw prime) (f/e y2-raw prime) a b)
                       (ec/inf a b))]]
      (testing (str "scalar multiplication for row " row)
       (is (= p2 (ec/scalar-mul p1 coeff)))))))
