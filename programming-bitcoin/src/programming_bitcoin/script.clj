(ns programming-bitcoin.script
  (:require [programming-bitcoin.encodings :as e]
            [programming-bitcoin.secp256k1 :as secp256k1]))

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

(def-ops {0 op-0
          76 op-pushdata1
          77 op-pushdata2
          78 op-pushdata4
          79 op-1negate
          81 op-1
          82 op-2
          83 op-3
          84 op-4
          85 op-5
          86 op-6
          87 op-7
          88 op-8
          89 op-9
          90 op-10
          91 op-11
          92 op-12
          93 op-13
          94 op-14
          95 op-15
          96 op-16
          97 op-nop
          105 op-verify
          118 op-dup
          135 op-equal
          136 op-equalverify
          147 op-add
          148 op-sub
          149 op-mul
          169 op-hash160
          170 op-hash256
          172 op-checksig
          173 op-checksigverify})

(def pushdata-bytes {op-pushdata1 1 op-pushdata2 2 op-pushdata4 4})

(defn- parse-op
  [bytes*]
  {:pre [(seq bytes*)]}
  ;; returns the new command and the number of bytes read
  (let [curr-byte (first bytes*)
        rest-bytes (rest bytes*)
        bytes-read 1]
    (assert (instance? Byte curr-byte))
    (cond (and (>= curr-byte (byte 1)) (<= curr-byte (byte 75)))
          [(take curr-byte rest-bytes) (+ bytes-read curr-byte)]
          (#{op-pushdata1 op-pushdata2 op-pushdata4} curr-byte)
          (let [n (pushdata-bytes curr-byte)
                data-len (e/bytes-lil-e->pos-biginteger (take n rest-bytes))]
            [(take data-len (drop n rest-bytes)) (+ bytes-read n data-len)])
          :else [curr-byte bytes-read])))

(defn parse
  "Parses a byte array into a `Script`. Returns the script and the unread
  bytes*."
  [bytes*]
  (let [[len bytes*] (e/read-varint bytes*)]
    (loop [cmds []
           total-bytes-read 0
           bytes* bytes*]
      (cond (> total-bytes-read len)
            (throw (ex-info "Parsing script failed"
                            {:len len :total-bytes-read total-bytes-read}))
            (= total-bytes-read len) [(->script cmds) bytes*]
            :else (let [[new-cmd bytes-read] (parse-op bytes*)]
                    (recur (conj cmds new-cmd)
                           (+ total-bytes-read bytes-read)
                           (drop bytes-read bytes*)))))))

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

(defn- eval-op-dispatch [op stack z] {:pre [(vector? stack)]} op)
(defmulti ^{:arglists (list ['op 'stack 'z])} eval-op
  "Performs a Script operation for the opcode `op`.

  Dispatches off the `op`, which should be a `byte`. The op will pop items from
  `stack` and return a new stack or `::halt`, which indicates the script is
  invalid and should stop execution."
  #'eval-op-dispatch)

(defn- op-helper
  [f n-args stack]
  {:pre [(vector? stack)]}
  (if (< (count stack) n-args)
    ::halt
    (let [split (- (count stack) n-args)
          args (subvec stack split)
          new-stack (subvec stack 0 split)]
      (apply f new-stack (reverse args)))))

(defmacro defn-op-0
  [op [stack-sym z-sym] & body]
  `(defmethod eval-op ~op
     [_# stack# ~z-sym]
     (op-helper (fn [~stack-sym] ~@body) 0 stack#)))

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

(defmacro number-op
  [n]
  (let [sym (symbol (str "op-" (Math/abs n) (when (< n 0) "negate")))]
    `(defn-op-0 ~sym [stack# _#] (conj stack# (encode-num ~n)))))

(defmacro arithmetic-op
  [op f]
  `(defn-op-2 ~op
     [stack# _# a# b#]
     (conj stack# (encode-num (~f (decode-num a#) (decode-num b#))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           op definitions                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(number-op -1)
(number-op 0)
(number-op 1)
(number-op 2)
(number-op 3)
(number-op 4)
(number-op 5)
(number-op 6)
(number-op 7)
(number-op 8)
(number-op 9)
(number-op 10)
(number-op 11)
(number-op 12)
(number-op 13)
(number-op 14)
(number-op 15)
(number-op 16)

(defn-op-0 op-nop [stack _] stack)
(defn-op-1 op-verify [stack _ v] (if (= (decode-num v) 0) ::halt stack))
(defn-op-1 op-dup [stack _ v] (conj stack v v))
(defn-op-2 op-equal [stack _ a b] (stack-bool stack (= (seq a) (seq b))))
(defn-op-> op-equalverify [op-equal op-verify])

(arithmetic-op op-add .add)
(arithmetic-op op-sub .subtract)
(arithmetic-op op-mul .multiply)

(defn-op-1 op-hash160 [stack _ v] (conj stack (e/hash160 v)))
(defn-op-1 op-hash256 [stack _ v] (conj stack (e/hash256 v)))

(defn-op-2 op-checksig
  [stack z pubkey-bytes sig-bytes]
  (let [sig (e/parse-der sig-bytes)
        pubkey (e/parse-sec pubkey-bytes)]
    (stack-bool stack (secp256k1/valid-signature? pubkey z sig))))

(defn-op-> op-checksigverify [op-checksig op-verify])

(defn evaluate
  [{:keys [cmds] :as script} ^BigInteger z]
  {:pre [(vector? cmds)]}
  (loop [stack []
         cmds cmds]
    (cond (= stack ::halt) false
          (seq cmds) (let [item (peek cmds)]
                       (recur (if (instance? Byte item)
                                (eval-op item stack z)
                                (conj stack item))
                              (pop cmds)))
          (not (seq stack)) false
          (zero? (decode-num (peek stack))) false
          :else true)))

(defn pprint
  [{:keys [cmds] :as script}]
  {:pre [(vector? cmds)]}
  (loop [cmds cmds]
    (when-let [item (peek cmds)]
      (if (instance? Byte item)
        (println (op-names item))
        (println (str "0x" (e/bytes->hex item))))
      (recur (pop cmds)))))

(comment (let [script-pubkey [(unchecked-byte 0x87) (unchecked-byte 0x56)
                              (unchecked-byte 0x93) (unchecked-byte 0x95)
                              (unchecked-byte 0x76) (unchecked-byte 0x76)]
               script-sig [(encode-num 2)]]
           (pprint {:cmds (into script-pubkey script-sig)})
           (evaluate {:cmds (into script-pubkey script-sig)} nil)))

(comment (prn *e))
