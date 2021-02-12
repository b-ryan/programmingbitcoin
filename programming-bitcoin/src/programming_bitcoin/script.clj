(ns programming-bitcoin.script
  (:require [programming-bitcoin.encodings :as e]))

(defrecord Script [cmds])
(defn ->script [cmds] (->Script cmds))

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
      (#{76 77} curr-byte)
      (let [n (if (= curr-byte 76) 1 2)
            data-len (e/bytes->pos-biginteger-le (take n rest-bytes))]
        [(take data-len (drop n rest-bytes)) (+ bytes-read n data-len)])
      :else [curr-byte bytes-read])))

(defn parse
  "Parses a byte array into a `Script`."
  [bytes*]
  (let [[len rest*] (e/read-varint bytes*)]
    (->script
     (loop [cmds []
            total-bytes-read 0
            curr-bytes rest*]
       (cond (> total-bytes-read len)
             (throw (ex-info "Parsing script failed"
                             {:len len :total-bytes-read total-bytes-read}))
             (= total-bytes-read len) cmds
             :else (let [[new-cmd bytes-read] (parse-op curr-bytes)]
                     (recur (conj cmds new-cmd)
                            (+ total-bytes-read bytes-read)
                            (drop bytes-read curr-bytes))))))))

(defn serialize
  "Encodes a `Script` to a byte sequence."
  [{:keys [cmds]}]
  (let [result
        (reduce
         (fn [accum cmd]
           (into
            accum
            (if (integer? cmd)
              (e/unsigned-bytes-le cmd)
              (let [cmd-len (count cmd)
                    lil-endi (comp e/unsigned-bytes-le biginteger)]
                (cond (<= cmd-len 75) (concat (lil-endi cmd-len) cmd)
                      (< cmd-len 0x100)
                      (concat (lil-endi 76) (lil-endi cmd-len) cmd)
                      (< cmd-len 520)
                      (concat (lil-endi 77) (lil-endi cmd-len) cmd)
                      :else (throw (ex-info "cmd too long" {:cmd cmd})))))))
         []
         cmds)]
    (concat (e/encode-varint (biginteger (count result))) result)))
