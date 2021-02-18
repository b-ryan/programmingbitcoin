(ns programming-bitcoin.script
  (:require [programming-bitcoin.encodings :as e]))

(defrecord Script [cmds])
(defn ->script [cmds] (->Script cmds))

(defmacro def-ops
  [ops]
  (let [op-names (into {}
                       (for [[v sym] ops]
                         [(unchecked-byte v)
                          (-> sym
                              name
                              (clojure.string/replace #"-" "_")
                              (clojure.string/upper-case))]))]
    (conj (for [[v sym] ops] `(def ~sym (unchecked-byte ~v)))
          `(def op-names ~op-names)
          'do)))

(def-ops {76 op-pushdata1
          77 op-pushdata2
          78 op-pushdata4
          105 op-verify
          118 op-dup
          135 op-equal
          136 op-equalverify})

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                             op helpers                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- encode-num [n] (if (zero? n) () (reverse (.toByteArray (biginteger n)))))

(defn- decode-num
  [bytes*]
  (if (seq bytes*) (BigInteger. (byte-array (reverse bytes*))) (biginteger 0)))

(def ^:private s-false (encode-num 0))
(def ^:private s-true (encode-num 1))

(defn- stack-bool [stack pred] (conj stack (if pred s-true s-false)))

(defn- eval-op-dispatch [op stack z] op)
(defmulti eval-op #'eval-op-dispatch)

(defn- op-helper
  [f n-args stack]
  (if (< (count stack) n-args)
    ::halt
    (let [split (- (count stack) n-args)
          args (subvec stack split)
          new-stack (subvec stack 0 split)]
      (apply f new-stack args))))

(defmacro defn-op-1
  [op [stack-sym z-sym arg1-sym] & body]
  `(defmethod eval-op ~op
     [_# stack# ~z-sym]
     (op-helper (fn [~stack-sym ~arg1-sym] ~@body) 1 stack#)))

(defmacro defn-op-2
  [op [stack-sym z-sym arg1-sym arg2-sym] & body]
  `(defmethod eval-op ~op
     [_# stack# ~z-sym]
     (op-helper (fn [~stack-sym ~arg1-sym ~arg2-sym] ~@body) 2 stack#)))

(defmacro defn-op->
  [op [& op-fns]]
  {:pre [(seq op-fns)]}
  (let [z-sym (gensym "z__")
        new-stack-sym (gensym "new-stack__")
        steps (map (fn [op]
                     `(if (= ~new-stack-sym ::halt)
                        ::halt
                        (eval-op ~op ~new-stack-sym ~z-sym)))
                   (rest op-fns))]
    `(defmethod eval-op ~op
       [_# stack# ~z-sym]
       (let [~new-stack-sym (eval-op ~(first op-fns) stack# ~z-sym)
             ~@(interleave (repeat new-stack-sym) steps)]
         ~new-stack-sym))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           op definitions                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn-op-1 op-verify [stack _ v] (if (= (decode-num v) 0) ::halt stack))
(defn-op-1 op-dup [stack _ v] (conj stack v v))
(defn-op-2 op-equal [stack _ a b] (stack-bool stack (= (seq a) (seq b))))
(defn-op-> op-equalverify [op-equal op-verify])
