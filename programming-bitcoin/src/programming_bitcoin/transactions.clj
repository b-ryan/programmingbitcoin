(ns programming-bitcoin.transactions
  (:require [clj-http.client :as http]
            [programming-bitcoin.encodings :as e]
            [programming-bitcoin.primitives :refer
             [biginteger-add biginteger-subtract]]
            [programming-bitcoin.script :as script]))

(defrecord TxIn [prev-tx prev-index script-sig sequence*])
(defrecord TxOut [amount script-pubkey])
(defrecord Transaction [version tx-ins tx-outs locktime])

(defn ->tx
  [version tx-ins tx-outs locktime]
  (->Transaction version tx-ins tx-outs locktime))

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

  Returns a `TxIn` and the unread bytes as a sequence."
  [bytes*]
  (let [[prev-tx-le bytes*] (take-drop 32 bytes*)
        [prev-index-le bytes*] (take-drop 4 bytes*)
        [script-sig bytes*] (script/parse bytes*)
        [sequence-le bytes*] (take-drop 4 bytes*)]
    [(->TxIn (e/bytes->hex (reverse prev-tx-le))
             (e/bytes-lil-e->pos-biginteger prev-index-le)
             script-sig
             (e/bytes-lil-e->pos-biginteger sequence-le)) bytes*]))

(defn- serialize-tx-in
  [{:keys [prev-tx prev-index script-sig sequence*] :as tx-in}]
  (concat (reverse (e/pad 32 (e/hex->bytes prev-tx)))
          (e/unsigned-bytes-lil-e prev-index 4)
          (script/serialize script-sig)
          (e/unsigned-bytes-lil-e sequence* 4)))

(defn- read-tx-out
  [bytes*]
  (let [[amount-le bytes*] (take-drop 8 bytes*)
        [script-pubkey bytes*] (script/parse bytes*)]
    [(->TxOut (e/bytes-lil-e->pos-biginteger amount-le) script-pubkey) bytes*]))

(defn- serialize-tx-out
  [{:keys [amount script-pubkey] :as tx-out}]
  (concat (e/unsigned-bytes-lil-e amount 8) (script/serialize script-pubkey)))

(defn parse
  [bytes*]
  (let [[version-bytes bytes*] (take-drop 4 bytes*)
        version (e/bytes-lil-e->pos-biginteger version-bytes)
        [tx-ins bytes*] (read-varint-things read-tx-in bytes*)
        [tx-outs bytes*] (read-varint-things read-tx-out bytes*)
        [locktime-le bytes*] (take-drop 4 bytes*)]
    (->tx version tx-ins tx-outs (e/bytes-lil-e->pos-biginteger locktime-le))))

(defn serialize
  [{:keys [version tx-ins tx-outs locktime sequence*] :as tx}]
  (concat (e/unsigned-bytes-lil-e version 4)
          (e/encode-varint (biginteger (count tx-ins)))
          (mapcat serialize-tx-in tx-ins)
          (e/encode-varint (biginteger (count tx-outs)))
          (mapcat serialize-tx-out tx-outs)
          (e/unsigned-bytes-lil-e locktime 4)))

(defn id
  "Returns the transaction ID, as a byte sequence."
  [tx]
  (-> tx
      serialize
      e/hash256
      reverse
      e/bytes->hex))

(defprotocol TxFetcher
 (fetch [this tx-id] "Fetches a transaction by its id."))

(defn- fetch-http-tx
  [tx-id testnet?]
  (let [raw (-> (format "%s/tx/%s.hex"
                        (if testnet?
                          "http://testnet.programmingbitcoin.com"
                          "http://mainnet.programmingbitcoin.com")
                        tx-id)
                http/get
                :body
                clojure.string/trim
                e/hex->bytes)
        tx (if (= (nth raw 4) 0)
             (-> (parse (concat (take 4 raw) (drop 6 raw)))
                 (assoc :locktime
                        (e/bytes-lil-e->pos-biginteger (take-last 4 raw))))
             (parse raw))
        calculated-id (id tx)]
    (when (not= tx-id calculated-id)
      (throw (ex-info "not the same id"
                      {:type ::http :expected tx-id :actual calculated-id})))
    tx))

(defrecord CachedHttpFetcher [cache opts]
 TxFetcher
   (fetch [{:keys [cache opts]} tx-id]
     (when (not (get @cache tx-id))
       (swap! cache assoc tx-id (fetch-http-tx tx-id (:testnet? opts))))
     (get @cache tx-id)))

(defn tx-in->prev-tx-output
  [{:keys [prev-tx prev-index]} tx-fetcher]
  (-> tx-fetcher
      (fetch prev-tx)
      :tx-outs
      (nth prev-index)))

(defn tx-in-amount
  [tx-in tx-fetcher]
  (:amount (tx-in->prev-tx-output tx-in tx-fetcher)))

(defn tx-in-script-pubkey
  [tx-in tx-fetcher]
  (:script-pubkey (tx-in->prev-tx-output tx-in tx-fetcher)))

(defn fee
  [{:keys [tx-ins tx-outs]} tx-fetcher]
  (let [out-amount (reduce biginteger-add (map :amount tx-outs))
        in-amount (reduce biginteger-add
                          (map #(tx-in-amount % tx-fetcher) tx-ins))]
    (biginteger-subtract in-amount out-amount)))

(comment (def h (->CachedHttpFetcher (atom {}) {:testnet? true})))

(comment
 (-> (fetch h
            "74ebcce0a52df5844b3f1f91f33e5b4845c3da5a9fa1db4dfa24e7a5f3514fdc")
     (fee h)))
