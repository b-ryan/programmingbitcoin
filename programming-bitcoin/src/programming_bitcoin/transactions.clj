(ns programming-bitcoin.transactions
  (:require [programming-bitcoin.encodings :as e]))

(defrecord Transaction [version tx-ins tx-outs locktime testnet?])

(defn ->tx
  [version tx-ins tx-outs locktime testnet?]
  (->Transaction version tx-ins tx-outs locktime testnet?))


(defn parse
  [bytes*]
  (let [version (->> bytes*
                     (take 4)
                     e/bytes->pos-biginteger-le)
        [num-ins rest*] (e/read-varint bytes*)]))
