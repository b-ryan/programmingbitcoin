(ns programming-bitcoin.encodings
  (:require [pandect.algo.ripemd160 :refer [ripemd160-bytes]]
            [pandect.algo.sha256 :refer [sha256-bytes]]
            [programming-bitcoin.secp256k1 :as s256]
            [programming-bitcoin.finite-fields :as ff]))

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

(defn- add-sign
  "Adds a leading byte 0 if the first item in `bytes*`'s first bit is not 0.

  Notably this means this only adds positive signs. Don't expect good things if
  you might be dealing with negatives."
  [bytes*]
  (if (= (bit-and (first bytes*) 0x80) 0) bytes* (cons (byte 0) bytes*)))

(defn hex
  "Converts collection of bytes `bytes*` to a hex string.

  Does not include 0x at the beginning."
  [bytes*]
  (reduce (fn [accum item] (str accum (format "%02x" item))) "" bytes*))

(defn hex->bytes
  "Converts a hex string into a sequence of bytes."
  [s]
  (remove-sign (.toByteArray (BigInteger. s 16))))

(def ^:private b58-alphabet
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

(defn base58
  "Converts collection of bytes `bytes*` to a base 58 string."
  [bytes*]
  (str
   (clojure.string/join (repeat (count (take-while (partial = 0) bytes*)) "1"))
   (loop [num* (BigInteger. (byte-array (add-sign bytes*)))
          result ""]
     (if (> num* 0)
       (let [[next-num mod-val] (.divideAndRemainder num* (biginteger 58))]
         (recur next-num (str (str (get b58-alphabet mod-val)) result)))
       result))))

(defn- pad
  "Left pads collection of bytes `bytes*` with byte 0 so `bytes*` has size `n`."
  [n bytes*]
  {:pre [(<= (count bytes*) n)]}
  (concat (repeat (- n (count bytes*)) (byte 0)) bytes*))

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

(defn sec-uncompressed
  "Encodes a `point` with uncompressed SEC format, where `:x` and `:y` of the
  `point` are finite fields."
  [{:keys [x y] :as point}]
  (cons (byte 4)
        (concat (biginteger->big-endian-32 (:number x))
                (biginteger->big-endian-32 (:number y)))))

(defn sec-compressed
  "Encodes a `point` with compressed SEC format, where `:x` and `:y` of the
  `point` are finite fields."
  [{:keys [x y] :as point}]
  (cons (byte (if (= (.mod (:number y) (biginteger 2)) 0) 2 3))
        (biginteger->big-endian-32 (:number x))))

(defn sec
  [point {:keys [compressed?]}]
  (if compressed? (sec-compressed point) (sec-uncompressed point)))

(defn- big-endian-32->biginteger
  [bytes*]
  (-> bytes*
      add-sign
      byte-array
      BigInteger.))

(defn parse-sec
  "Parses an SEC formatted string into a secp256k1 point."
  [bytes*]
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
      (s256/p x y))))

#_(->> (.pow (biginteger 999) (biginteger 3))
       programming-bitcoin.bitcoin-curve/secret->private-key
       :point
       sec-uncompressed
       parse)


(defn der
  [{:keys [r s] :as signature}]
  (letfn [(cons-count [bytes*] (cons (count bytes*) bytes*))
          (cons-marker [bytes*] (cons (byte 0x02) bytes*))
          (cons-start [bytes*] (cons (byte 0x30) bytes*))]
    (-> (concat (-> r
                    .toByteArray
                    cons-count
                    cons-marker)
                (-> s
                    .toByteArray
                    cons-count
                    cons-marker))
        cons-count
        cons-start)))

(defn parse-der
  [bytes*]
  (let [curr (drop 3 bytes*)
        [r-len curr] [(first curr) (drop 1 curr)]
        [r-bytes curr] [(take r-len curr) (drop (inc r-len) curr)]
        [s-len curr] [(first curr) (drop 1 curr)]
        s-bytes (take s-len curr)]
    (s256/->sig (BigInteger. (byte-array r-bytes))
                (BigInteger. (byte-array s-bytes)))))


(def sha256 (comp sha256-bytes byte-array))
(def ripemd160 (comp ripemd160-bytes byte-array))

(def ^{:doc "Two rounds of sha256"} hash256 (comp sha256 sha256))
(def ^{:doc "sha256 followed by ripemd160"} hash160 (comp ripemd160 sha256))

(defn base58-with-checksum
  "Adds 4 bytes of the hash256 checksum and then encodes using base 58."
  [bytes*]
  (base58 (concat bytes* (take 4 (hash256 bytes*)))))

(defn point->hash160
  "Does hash160 on an SEC formatted point (compressed or uncompressed)."
  ([point] (point->hash160 point nil))
  ([point {:keys [compressed?] :or {compressed? true}}]
   (hash160 (sec point {:compressed? compressed?}))))

(defn point->address
  "Converts a public key point to a Bitcoin address (as a string)."
  ([point] (point->address point nil))
  ([point {:keys [testnet? compressed?] :or {testnet? false compressed? true}}]
   (base58-with-checksum (cons (byte (if testnet? 0x6f 0))
                               (point->hash160 point
                                               {:compressed? compressed?})))))
