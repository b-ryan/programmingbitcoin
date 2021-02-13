(ns programming-bitcoin.encodings
  (:require [pandect.algo.ripemd160 :refer [ripemd160-bytes]]
            [pandect.algo.sha256 :refer [sha256-bytes]]
            [programming-bitcoin.secp256k1 :as s256]
            [programming-bitcoin.finite-fields :as ff])
  (:import org.apache.commons.codec.binary.Hex))

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

(defn bytes->hex
  "Converts collection of bytes `bytes*` to a hex string.

  Does not include 0x at the beginning."
  [bytes*]
  (reduce (fn [accum item] (str accum (format "%02x" item))) "" bytes*))

(def ^{:doc "An alias for `bytes->hex`"} hex bytes->hex)

(defn hex->bytes
  "Converts a hex string into a sequence of bytes."
  [s]
  (Hex/decodeHex (.toCharArray s)))

(defn bytes->pos-biginteger
  "Converts a big-endian vector of bytes, representing a non-negative integer,
  to a BigInteger.

  The number is assumed to be positive. Since the BigInteger. constructor
  expects two's complement byte arrays, the 0 byte will be added when necessary
  to ensure the resulting BigInteger is non-negative."
  [bytes*]
  (-> bytes*
      add-sign
      byte-array
      BigInteger.))

(defn bytes-lil-e->pos-biginteger
  "Converts a little-endian vector of bytes, representing a non-negative
  integer, to a BigInteger.

  The number is assumed to be positive. Since the BigInteger. constructor
  expects two's complement byte arrays, the 0 byte will be added when necessary
  to ensure the resulting BigInteger is non-negative."
  [bytes*]
  (-> bytes*
      reverse
      bytes->pos-biginteger))

(def ^:private b58-alphabet
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

(defn base58
  "Converts collection of bytes `bytes*` to a base 58 string."
  [bytes*]
  (str
   (clojure.string/join (repeat (count (take-while (partial = 0) bytes*)) "1"))
   (loop [num* (bytes->pos-biginteger bytes*)
          result ""]
     (if (> num* 0)
       (let [[next-num mod-val] (.divideAndRemainder num* (biginteger 58))]
         (recur next-num (str (str (get b58-alphabet mod-val)) result)))
       result))))

(defn- pad
  "Left pads collection of bytes `bytes*` with byte 0 so `bytes*` has size `n`."
  [n bytes*]
  (concat (repeat (- n (count bytes*)) (byte 0)) bytes*))

(defn unsigned-bytes
  "Returns a big-endian byte sequence for BigInteger `x`, not including a sign
  bit.

  This function only works for positive BigInteger values."
  ([^BigInteger x] (unsigned-bytes x 0))
  ([^BigInteger x pad*]
   {:pre [(>= x (biginteger 0))]}
   (->> x
        .toByteArray
        remove-sign
        (pad pad*))))

(defn unsigned-bytes-lil-e
  "Returns a little-endian byte sequence for BigInteger `x`, not including a
  sign bit.

  This function only works for positive BigInteger values."
  ([^BigInteger x] (unsigned-bytes x 0))
  ([^BigInteger x pad*]
   {:pre [(>= x (biginteger 0))]}
   (->> x
        unsigned-bytes
        reverse)))

(defn- unsigned-bytes-32
  "Converts BigInteger `x` to big-endian, returning a vector of 32 bytes.

  This function expects a positive integer and it will not include the two's
  complement sign bit.

  We are assuming you have not given a BigInteger which is too big for 32
  bytes. If you do, the behavior is undefined, but an exception will likely be
  thrown."
  [^BigInteger x]
  {:pre [(>= x (biginteger 0))]}
  (unsigned-bytes x 32))

(defn sec-uncompressed
  "Encodes a `point` with uncompressed SEC format, where `:x` and `:y` of the
  `point` are finite fields."
  [{:keys [x y] :as point}]
  (cons (byte 4)
        (concat (unsigned-bytes-32 (:number x))
                (unsigned-bytes-32 (:number y)))))

(defn sec-compressed
  "Encodes a `point` with compressed SEC format, where `:x` and `:y` of the
  `point` are finite fields."
  [{:keys [x y] :as point}]
  (cons (byte (if (= (.mod (:number y) (biginteger 2)) 0) 2 3))
        (unsigned-bytes-32 (:number x))))

(defn sec
  [point {:keys [compressed?]}]
  (if compressed? (sec-compressed point) (sec-uncompressed point)))

(defn parse-sec
  "Parses an SEC formatted string into a secp256k1 point."
  [bytes*]
  (if (= (first bytes*) 4)
    (s256/p (->> bytes*
                 (drop 1)
                 (take 32)
                 bytes->pos-biginteger)
            (->> bytes*
                 (drop 33)
                 (take 32)
                 bytes->pos-biginteger))
    (let [x (->> bytes*
                 (drop 1)
                 bytes->pos-biginteger
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
    ;; Unlike many other encodings in this ns, DER signatures DO use two's
    ;; complement. So the number can be expected to be signed and fit right
    ;; into the BigInteger. constructor.
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

(defn wif
  "Encodes a secret using Wallet Import Format (WIF)."
  ([secret] (wif secret nil))
  ([secret {:keys [testnet? compressed?] :or {testnet? false compressed? true}}]
   {:pre [(integer? secret)]}
   (base58-with-checksum (concat (list (unchecked-byte (if testnet? 0xef 0x80)))
                                 (unsigned-bytes-32 (biginteger secret))
                                 (when compressed? (list (byte 1)))))))

(defn read-varint
  [bytes*]
  (let [fst (first bytes*)
        read-bytes (fn [n] [(->> bytes*
                                 (drop 1)
                                 (take n)
                                 reverse
                                 bytes->pos-biginteger) (drop (inc n) bytes*)])]
    (condp #(= (unchecked-byte %1) %2) (first bytes*)
      0xfd (read-bytes 2)
      0xfe (read-bytes 4)
      0xff (read-bytes 8)
      [(biginteger fst) (rest bytes*)])))

(comment (read-varint (map unchecked-byte [23])))
(comment (read-varint (map unchecked-byte [0xfe 88 99 20 3])))

(defn encode-varint
  [^BigInteger x]
  (letfn [(encode [prefix padding]
                  (cons (unchecked-byte prefix)
                        (-> x
                            (unsigned-bytes padding)
                            reverse)))]
    (condp #(< %2 (biginteger %1)) x
      0xfd (unsigned-bytes x)
      0x10000 (encode 0xfd 2)
      0x100000000 (encode 0xfe 4)
      0x10000000000000000 (encode 0xff 8)
      (assert false "Number too big"))))
