(ns programming-bitcoin.secp256k1
  (:require [programming-bitcoin.finite-fields :as ff]
            [programming-bitcoin.elliptic-curves :as ec]
            [programming-bitcoin.primitives :refer
             [biginteger? rand-biginteger]]))

(def
  ^{:private true
    :doc
    "The prime number, as a BigInteger, defining the secp256k1 finite field."}
  ^BigInteger P
  (-> (.pow (biginteger 2) (biginteger 256))
      (.subtract (.pow (biginteger 2) (biginteger 32)))
      (.subtract (biginteger 977))))

(def
  ^{:doc
    "The secp256k1 value, as a BigInteger, for `a` in the elliptic curve
  formula y^2 = x^3 + a*x + b"}
  A
  (biginteger 0))

(def
  ^{:doc
    "The secp256k1 value, as a BigInteger, for `b` in the elliptic curve
  formula y^2 = x^3 + a*x + b"}
  B
  (biginteger 7))

(defn e
  "Creates a programming-bitcoin.finite-fields/Element with the secp256k1
  prime, `P`."
  [number]
  (ff/e number P));; TODO rename to ->e or longer name

(defn p ;; TODO rename
  "Creates a programming-bitcoin.elliptic-curves/Point on the secp256k1 curve.

  `x` and `y` can either be `Field` instances or integers. When integers, they
  will be coerced to a BigInteger for you."
  [x y]
  (ec/p (if (integer? x) (e (biginteger x)) x)
        (if (integer? y) (e (biginteger y)) y)
        (e A)
        (e B)))

(def ^{:doc "Generator point of the secp256k1 group."} G
  (p 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
     0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8))

(def
  ^{:doc
    "The order of the group generated by G.

  Ie. the number of points in the group. G * n = point at infinity."}
  ^BigInteger N
  (biginteger
   0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141))

#_(clojure.pprint/pprint (ec/scalar-mul G N))

(defrecord Signature [r s])
(defn ->sig [r s] (->Signature (biginteger r) (biginteger s)))

(defn valid-signature?
  "Returns boolean indicating whether signature `signature` for signature hash
  `z` is a valid signature for the public key `point`."
  [{:keys [^BigInteger r ^BigInteger s] :as signature} point ^BigInteger z]
  (let [s-inv (.modPow s (.subtract N (biginteger 2)) N)
        u (.mod (.multiply z s-inv) N)
        v (.mod (.multiply r s-inv) N)
        total (ec/add (ec/scalar-mul G u) (ec/scalar-mul point v))]
    (= (:number (:x total)) r)))

#_(let [point
        (p 0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c
           0x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34)
        z (biginteger
           0xec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60)
        r (biginteger
           0xac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395)
        s (biginteger
           0x68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4)
        sig (->sig r s)]
    (valid-signature? sig point z))

(defrecord PrivateKey [secret point])

(defn secret->private-key
  "Converts a BigInteger `secret` into a `PrivateKey`, which contains the
  secret and the public key point."
  [secret]
  {:pre [(integer? secret)]}
  (->PrivateKey (biginteger secret) (ec/scalar-mul G secret)))

(defn rand-secret
  "Returns a random BigInteger between 0 and `N`."
  []
  (rand-biginteger N))

(defn sign
  [{:keys [^BigInteger secret point] :as private-key} z]
  {:pre [(integer? z)]}
  (let [z (biginteger z)
        k (rand-secret)
        r (:number (:x (ec/scalar-mul G k)))
        k-inv (.modPow k (.subtract N (biginteger 2)) N)
        s (-> (.multiply secret r)
              (.add z)
              (.multiply k-inv)
              (.mod N))]
    (->sig r (if (> s (.divide N (biginteger 2))) (.subtract N s) s))))

(defn elem-sqrt
  "Calculates the square root of the finite field element `elem` which must be
  on the curve defined by this namespace."
  [elem]
  (ff/pow elem (.divide (.add P (biginteger 1)) (biginteger 4))))

(defn calc-y
  "Given `x`, calculates `y` based on the secp256k1 elliptic curve and whether
  you want the even or odd `y`."
  [x {:keys [want-even?]}]
  (let [alpha (ff/add (ff/pow x 3) (e B))
        {:keys [number] :as beta} (elem-sqrt alpha)
        is-even? (= (.mod number (biginteger 2)) (biginteger 0))]
    (if (= is-even? want-even?) beta (e (.subtract P number)))))
