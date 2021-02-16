(ns programming-bitcoin.script
  (:require [programming-bitcoin.encodings :as e]))

(defrecord Script [cmds])
(defn ->script [cmds] (->Script cmds))

(defmacro defops
  [ops]
  (let [op-names (into {}
                       (for [[v sym] ops]
                         [v
                          (-> sym
                              name
                              (clojure.string/replace #"-" "_")
                              (clojure.string/upper-case))]))]
    (conj
     (for [[v sym] ops] `(def ~sym ~v))
     `(def op-names ~op-names)
     'do)))

(defops
  {76 op-pushdata1
   77 op-pushdata2
   78 op-pushdata4})

(def pushdata-bytes {op-pushdata1 1 op-pushdata2 2 op-pushdata4 4})

(defn- parse-op
  [bytes*]
  ;; returns the new command and the number of bytes read
  (let [curr-byte (first bytes*)
        rest-bytes (rest bytes*)
        bytes-read 1]
    (cond
      ;; note that curr-byte is a byte, so integer comparison doesn't
      ;; always work as you'd expect (since bytes are signed). But
      ;; comparing to ints 1 and 75 work just fine.
      (and (>= curr-byte 1) (<= curr-byte 75)) [(take curr-byte rest-bytes)
                                                (+ bytes-read curr-byte)]
      (#{op-pushdata1 op-pushdata2 op-pushdata4} curr-byte)
      (let [n (pushdata-bytes curr-byte)
            data-len (e/bytes-lil-e->pos-biginteger (take n rest-bytes))]
        [(take data-len (drop n rest-bytes)) (+ bytes-read n data-len)])
      :else [curr-byte bytes-read])))

(defn parse
  "Parses a byte array into a `Script`. Returns the script and the unread
  bytes*."
  [bytes*]
  (let [[len rest*] (e/read-varint bytes*)]
    (loop [cmds []
           total-bytes-read 0
           rest* rest*]
      (cond (> total-bytes-read len)
            (throw (ex-info "Parsing script failed"
                            {:len len :total-bytes-read total-bytes-read}))
            (= total-bytes-read len) [(->script cmds) rest*]
            :else (let [[new-cmd bytes-read] (parse-op rest*)]
                    (recur (conj cmds new-cmd)
                           (+ total-bytes-read bytes-read)
                           (drop bytes-read rest*)))))))

(defn serialize
  "Encodes a `Script` to a byte sequence."
  [{:keys [cmds]}]
  (let [result
        (reduce
         (fn [accum cmd]
           (into
            accum
            (if (instance? Byte cmd)
              [cmd]
              (let [cmd-len (count cmd)
                    lil-endi (comp e/unsigned-bytes-lil-e biginteger)]
                (cond (<= cmd-len 75) (concat (lil-endi cmd-len) cmd)
                      (< cmd-len 0x100)
                      (concat (lil-endi 76) (lil-endi cmd-len) cmd)
                      (< cmd-len 520)
                      (concat (lil-endi 77) (lil-endi cmd-len) cmd)
                      :else (throw (ex-info "cmd too long" {:cmd cmd})))))))
         []
         cmds)]
    (concat (e/encode-varint (biginteger (count result))) result)))
