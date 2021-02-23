(ns programming-bitcoin.transactions
  (:require [clj-http.client :as http]
            [programming-bitcoin.encodings :as e]
            [programming-bitcoin.primitives :refer
             [biginteger-add biginteger-subtract biginteger?]]
            [programming-bitcoin.script :as script]))

(defrecord TxIn [prev-tx prev-index script-sig sequence*])
(defrecord TxOut [amount script-pubkey])
(defrecord Transaction [version tx-ins tx-outs locktime])

(def empty-script (script/->script []))
(defn- or-nil? [f x] (or (nil? x) (f x)))

(defn ->tx-in
  [prev-tx prev-index script-sig sequence*]
  {:pre [(string? prev-tx) (biginteger? prev-index)
         (or-nil? biginteger? sequence*)]}
  (->TxIn prev-tx
          prev-index
          (or script-sig empty-script)
          (or sequence* (biginteger 0xffffffff))))

(defn ->tx-out
  [amount script-pubkey]
  {:pre [(integer? amount)]}
  (->TxOut amount (or script-pubkey empty-script)))

(defn ->tx
  "version: `BigInteger`
  tx-ins: coll of `TxIn`
  tx-outs: coll of `TxOut`
  locktime: `BigInteger`"
  [version tx-ins tx-outs locktime]
  (->Transaction version tx-ins tx-outs locktime))

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
  (let [[prev-tx-le bytes*] (split-at 32 bytes*)
        [prev-index-le bytes*] (split-at 4 bytes*)
        [script-sig bytes*] (script/parse bytes*)
        [sequence-le bytes*] (split-at 4 bytes*)]
    [(->tx-in (e/bytes->hex (reverse prev-tx-le))
              (e/bytes-lil-e->pos-biginteger prev-index-le)
              script-sig
              (e/bytes-lil-e->pos-biginteger sequence-le)) bytes*]))

(defn- serialize-tx-in
  [{:keys [prev-tx prev-index script-sig sequence*] :as tx-in}]
  (concat (reverse (e/pad 32 (e/hex->bytes prev-tx)))
          (e/BI->bytes prev-index {:pad 4 :signed? false :endian :little})
          (script/serialize script-sig)
          (e/BI->bytes sequence* {:pad 4 :signed? false :endian :little})))

(defn- read-tx-out
  [bytes*]
  (let [[amount-le bytes*] (split-at 8 bytes*)
        [script-pubkey bytes*] (script/parse bytes*)]
    [(->tx-out (e/bytes-lil-e->pos-biginteger amount-le) script-pubkey)
     bytes*]))

(defn- serialize-tx-out
  [{:keys [amount script-pubkey] :as tx-out}]
  (concat (e/BI->bytes amount {:pad 8 :signed? false :endian :little})
          (script/serialize script-pubkey)))

(defn parse
  [bytes*]
  (let [[version-bytes bytes*] (split-at 4 bytes*)
        version (e/bytes-lil-e->pos-biginteger version-bytes)
        [tx-ins bytes*] (read-varint-things read-tx-in bytes*)
        [tx-outs bytes*] (read-varint-things read-tx-out bytes*)
        [locktime-le bytes*] (split-at 4 bytes*)]
    (->tx version tx-ins tx-outs (e/bytes-lil-e->pos-biginteger locktime-le))))

(defn serialize
  [{:keys [version tx-ins tx-outs locktime sequence*] :as tx}]
  {:post [(vector? %)]}
  (into []
        (concat (e/unsigned-bytes-lil-e version 4)
                (e/encode-varint (biginteger (count tx-ins)))
                (mapcat serialize-tx-in tx-ins)
                (e/encode-varint (biginteger (count tx-outs)))
                (mapcat serialize-tx-out tx-outs)
                (e/unsigned-bytes-lil-e locktime 4))))

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

(def sighash-all 1)
(def sighash-none 2)
(def sighash-single 3)
(def sighash-anyonecanpay 0x80)

(defn sighash
  "Returns the hash that needs to get signed for index `input-index` as a
  `BigInteger`."
  [tx tx-fetcher input-index]
  (-> tx
      (update :tx-ins
              (fn [tx-ins]
                (map-indexed
                 (fn [idx tx-in]
                   (assoc tx-in
                          :script-sig
                          (if (= idx input-index)
                            (tx-in-script-pubkey tx-in tx-fetcher)
                            empty-script)))
                 tx-ins)))
      (serialize)
      (into (e/BI->bytes (biginteger sighash-all)
                         {:pad 4 :endian :little :signed? false}))
      (e/hash256)
      (e/bytes->pos-biginteger)))

(defn verify
  [{:keys [tx-ins] :as tx} tx-fetcher]
  (and (>= (fee tx tx-fetcher) 0)
       (every?
        identity
        (map-indexed (fn [idx tx-in]
                       (script/evaluate (script/combine
                                         (tx-in-script-pubkey tx-in tx-fetcher)
                                         (:script-sig tx-in))
                                        (sighash tx tx-fetcher idx)))
                     tx-ins))))

(comment (def h (->CachedHttpFetcher (atom {}) {:testnet? true})))
(comment (def p (->CachedHttpFetcher (atom {}) {:testnet? false})))

(comment
 (let [tx (fetch
           h
           "74ebcce0a52df5844b3f1f91f33e5b4845c3da5a9fa1db4dfa24e7a5f3514fdc")
       ;; one input
       in (first (:tx-ins tx))
       ;; two outputs, the second is the one to me
       out (-> tx
               (:tx-outs)
               (second))]
   (script/pprint (:script-pubkey out))))

(comment
 (-> (fetch h
            "74ebcce0a52df5844b3f1f91f33e5b4845c3da5a9fa1db4dfa24e7a5f3514fdc")
     (valid-amounts? h)))

(comment
 (let
   [tx-hex
    "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"
    tx (parse (e/hex->bytes tx-hex))
    expected-sighash
    "27e0c5994dec7824e56dec6b2fcb342eb7cdb0d0957c2fce9882f715e85d81a6"]
   #_(sighash tx p (byte 1))
   (verify tx p)))

(comment
 (-> (fetch p
            "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b")
     (valid-amounts? h)))
