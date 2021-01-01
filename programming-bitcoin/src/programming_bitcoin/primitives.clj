(ns programming-bitcoin.primitives
  (:require [clojure.spec.alpha :as s]))

(defprotocol Primitive
 (add [a b] "Adds a + b")
 (sub [a b] "Subtracts a - b")
 (mul [a b] "Multiplies a * b")
 (pow [a x] "Raises a to the power of x")
 (div [a b] "Divides a by b")
 (scalar-mul [a b] "Multiplies a * b where b is a scalar"))

(defmacro ^:private wrap-biginteger
  [func]
  (let [wrapped-name (symbol (str "biginteger-" (subs (name func) 1)))]
    `(do (defn ~wrapped-name
           [^BigInteger this# other#]
           (~func this# (biginteger other#)))
         #'~wrapped-name)))

(extend BigInteger
 Primitive
   {:add (wrap-biginteger .add)
    :sub (wrap-biginteger .subtract)
    :mul (wrap-biginteger .multiply)
    :pow (wrap-biginteger .pow)
    :div (wrap-biginteger .divide)
    :scalar-mul #'biginteger-multiply})

;; TODO should mod-pow be in the interface rather than pow? Or in addition to?

(s/def ::primitive (s/with-gen any? (s/gen integer?)))

(defn biginteger? [x] (instance? BigInteger x))
