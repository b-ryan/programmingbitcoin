(ns programming-bitcoin.encodings
  (:require [programming-bitcoin.secp256k1 :as s256]
            [programming-bitcoin.finite-fields :as ff]))

(defn- pad
  "Left pads collection of bytes `bytes*` with byte 0 so `bytes*` has size `n`."
  [n bytes*]
  {:pre [(<= (count bytes*) n)]}
  (concat (repeat (- n (count bytes*)) (byte 0)) bytes*))

(defn- remove-sign
  "Removes the first item from `bytes*` if it is the byte 0.

  This assumes `bytes*` is a collection of bytes generated with .toByteArray,
  which returns a two's complement, big-endian byte array. We also assume that
  it's a non-negative number we're dealing with.

  The first byte in `bytes*` will be 0 if the byte was added just for the sign
  bit. For example, 0xf0 in binary has a 1 as the left-most bit. In order to
  add the sign bit, a new byte needs to be added, so the final result will be
  0x00f0. We don't want that leading empty byte, so this strips it out."
  [bytes*]
  (if (= (first bytes*) 0) (rest bytes*) bytes*))

(defn- biginteger->big-endian-32
  "Converts BigInteger `x` to big-endian, returning a vector of 32 bytes.

  We are assuming you have not given a BigInteger which is too big for 32
  bytes. If you do, the behavior is undefined, but an exception will likely be
  thrown."
  [^BigInteger x]
  {:pre [(>= (.compareTo x (biginteger 0)) 0)]}
  (->> (.toByteArray x)
       (into [])
       remove-sign
       (pad 32)))

(defn- bytes->hex-str
  "Converts collection of bytes `bytes*` to a hex string.

  Does not include 0x at the beginning."
  [bytes*]
  (reduce (fn [accum item] (str accum (format "%02x" item))) "" bytes*))

(defn sec-uncompressed
  "Encodes a `point` with uncompressed SEC format, where `:x` and `:y` of the
  `point` are finite fields."
  [{:keys [x y] :as point}]
  (bytes->hex-str (cons (byte 4)
                        (concat (biginteger->big-endian-32 (:number x))
                                (biginteger->big-endian-32 (:number y))))))

(defn sec-compressed
  "Encodes a `point` with compressed SEC format, where `:x` and `:y` of the
  `point` are finite fields."
  [{:keys [x y] :as point}]
  (bytes->hex-str (cons (byte (if (= (.mod (:number y) (biginteger 2)) 0) 2 3))
                        (biginteger->big-endian-32 (:number x)))))

(defn- add-sign
  [bytes*]
  (if (= (bit-and (first bytes*) 2r10000000) 0) bytes* (cons (byte 0) bytes*)))

(defn- big-endian-32->biginteger
  [bytes*]
  (-> bytes*
      add-sign
      byte-array
      BigInteger.))

(defn parse-sec
  "Parses an SEC formatted string into a secp256k1 point."
  [s]
  (let [bytes* (.toByteArray (BigInteger. s 16))]
    (if (= (first bytes*) 4)
      (s256/p (->> bytes*
                   (drop 1)
                   (take 32)
                   big-endian-32->biginteger)
              (->> bytes*
                   (drop 33)
                   (take 32)
                   big-endian-32->biginteger))
      (let [x (->> bytes*
                   (drop 1)
                   big-endian-32->biginteger
                   s256/e)
            y (s256/calc-y x {:want-even? (= (first bytes*) 2)})]
        (s256/p x y)))))

#_(def X
    (->> (.pow (biginteger 999) (biginteger 3))
         programming-bitcoin.bitcoin-curve/secret->private-key
         :point
         sec-uncompressed))

#_(parse X)


(defn der
  [{:keys [r s] :as signature}]
  (letfn [(cons-count [bytes*] (cons (count bytes*) bytes*))
          (cons-marker [bytes*] (cons (byte 0x02) bytes*))
          (cons-start [bytes*] (cons (byte 0x30) bytes*))]
    (-> (concat
         (-> r
             .toByteArray
             cons-count
             cons-marker)
         (-> s
             .toByteArray
             cons-count
             cons-marker))
        cons-count
        cons-start
        bytes->hex-str)))

(defn parse-der
  [s]
  (let [bytes* (.toByteArray (BigInteger. s 16))
        curr (drop 3 bytes*)
        [r-len curr] [(first curr) (drop 1 curr)]
        [r-bytes curr] [(take r-len curr) (drop (inc r-len) curr)]
        [s-len curr] [(first curr) (drop 1 curr)]
        s-bytes (take s-len curr)]
    (s256/->sig
     (BigInteger. (byte-array r-bytes))
     (BigInteger. (byte-array s-bytes)))))
