(ns programming-bitcoin.transactions
  (:require [programming-bitcoin.encodings :as e]
            [programming-bitcoin.script :as script]))

(defrecord TxIn [prev-tx prev-index script-sig sequence*])
(defrecord TxOut [amount script-pubkey])
(defrecord Transaction [version tx-ins tx-outs locktime testnet?])

(defn ->tx
  [version tx-ins tx-outs locktime testnet?]
  (->Transaction version tx-ins tx-outs locktime testnet?))

(defn- take-drop [n coll] [(take n coll) (drop n coll)])

(defn- read-varint-things
  [f bytes*]
  (let [[n bytes*] (e/read-varint bytes*)]
    (loop [accum []
           bytes* bytes*
           x 0]
      (if (= x n)
        [accum bytes*]
        (let [[thing unread-bytes] (f bytes*)]
          (recur (conj accum thing) unread-bytes (inc x)))))))

(defn- read-tx-in
  "Reads a single input from `bytes*`, returning the fields of the input and
  the remaining bytes.

  Returns a `TxIn` and the unread bytes as a sequence.

  The transaction ID bytes are returned in big-endian."
  [bytes*]
  (let [[prev-tx-le bytes*] (take-drop 32 bytes*)
        [prev-index-le bytes*] (take-drop 4 bytes*)
        [script-sig bytes*] (script/parse bytes*)
        [sequence-le bytes*] (take-drop 4 bytes*)]
    [(->TxIn (reverse prev-tx-le)
             (e/bytes-lil-e->pos-biginteger prev-index-le)
             script-sig
             (e/bytes-lil-e->pos-biginteger sequence-le)) bytes*]))

(defn- read-tx-out
  [bytes*]
  (let [[amount-le bytes*] (take-drop 8 bytes*)
        [script-pubkey bytes*] (script/parse bytes*)]
    [(->TxOut (e/bytes-lil-e->pos-biginteger amount-le) script-pubkey) bytes*]))

(defn parse
  [bytes*]
  (let [[version-bytes bytes*] (take-drop 4 bytes*)
        version (e/bytes-lil-e->pos-biginteger version-bytes)
        [tx-ins bytes*] (read-varint-things read-tx-in bytes*)
        [tx-outs bytes*] (read-varint-things read-tx-out bytes*)
        [locktime-le bytes*] (take-drop 4 bytes*)
        testnet? false]
    (->tx version
          tx-ins
          tx-outs
          (e/bytes-lil-e->pos-biginteger locktime-le)
          testnet?)))
