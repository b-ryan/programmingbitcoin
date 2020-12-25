(ns programming-bitcoin.primitives
  (:require [clojure.math.numeric-tower :refer [expt]]
            [clojure.spec.alpha :as s]))

(defprotocol Primitive
  (add [a b] "Adds a + b")
  (sub [a b] "Subtracts a - b")
  (mul [a b] "Multiplies a * b")
  (pow [a x] "Raises a to the power of x")
  (div [a b] "Divides a by b")
  (scalar-mul [a b] "Multiplies a * b where b is a scalar"))

(extend Number
  Primitive
    {:add + :sub - :mul * :pow expt :div / :scalar-mul *})

(s/def ::primitive (s/with-gen any? (s/gen int?)))
