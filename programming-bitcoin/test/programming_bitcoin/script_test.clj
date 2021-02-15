(ns programming-bitcoin.script-test
  (:require [clojure.test :refer [deftest testing is]]
            [programming-bitcoin.encodings :as e]
            [programming-bitcoin.script :as s]))

(deftest parse
  (let
    [bytes*
     (e/hex->bytes
      "6a47304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a7160121035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937")
     script (first (s/parse bytes*))
     want1
     "304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a71601"
     want2 "035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937"]
    (is (= want1 (e/bytes->hex (first (:cmds script)))))
    (is (= want2 (e/bytes->hex (second (:cmds script)))))))

(deftest serialize
  (let
    [want
     "6a47304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a7160121035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937"
     script (first (s/parse (e/hex->bytes want)))]
    (is (= want (e/bytes->hex (s/serialize script))))))

(deftest op-dup
  (is (= [(byte 1) (byte 1)] (s/eval-op s/op-dup [(byte 1)] nil))))

(deftest op-equal-and-verify
  (letfn [(b [v] [(byte v)])]
    (is (= [()] (s/eval-op s/op-equal [(b 2) (b 3)] nil)))
    (is (= [(b 2)] (s/eval-op s/op-verify [(b 2) (b 3)] nil)))
    (is (= [(b 8)] (s/eval-op s/op-equalverify [(b 8) (b 2) (b 2)] nil)))
    (is (= ::s/halt (s/eval-op s/op-equalverify [(b 2) (b 3)] nil)))))

(deftest checksig
  (let
    [z (biginteger
        0x7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d)
     sec
     (e/hex->bytes
      "04887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34")
     sig
     (e/hex->bytes
      "3045022000eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c022100c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab601")
     script-pukey [(unchecked-byte 0xac) sec]
     script-sig [sig]
     combined-script (s/->script (into script-pukey script-sig))]
    (is (s/evaluate combined-script z))))
