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
     want2
     "304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a71601"
     want1 "035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937"]
    (is (= want1 (e/bytes->hex (first (:cmds script)))))
    (is (= want2 (e/bytes->hex (second (:cmds script)))))))

(deftest serialize
  (let
    [want
     "6a47304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a7160121035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937"
     script (first (s/parse (e/hex->bytes want)))]
    (is (= want (e/bytes->hex (s/serialize script))))))

(deftest op-nums
  (is (= [()] (s/eval-op s/op-0 [] nil)))
  (is (= [[(byte -1)]] (s/eval-op s/op-1negate [] nil)))
  (is (= [[(byte 1)]] (s/eval-op s/op-1 [] nil)))
  (is (= [[(byte 2)]] (s/eval-op s/op-2 [] nil)))
  (is (= [[(byte 3)]] (s/eval-op s/op-3 [] nil)))
  (is (= [[(byte 4)]] (s/eval-op s/op-4 [] nil)))
  (is (= [[(byte 5)]] (s/eval-op s/op-5 [] nil)))
  (is (= [[(byte 6)]] (s/eval-op s/op-6 [] nil)))
  (is (= [[(byte 7)]] (s/eval-op s/op-7 [] nil)))
  (is (= [[(byte 8)]] (s/eval-op s/op-8 [] nil)))
  (is (= [[(byte 9)]] (s/eval-op s/op-9 [] nil)))
  (is (= [[(byte 10)]] (s/eval-op s/op-10 [] nil)))
  (is (= [[(byte 11)]] (s/eval-op s/op-11 [] nil)))
  (is (= [[(byte 12)]] (s/eval-op s/op-12 [] nil)))
  (is (= [[(byte 13)]] (s/eval-op s/op-13 [] nil)))
  (is (= [[(byte 14)]] (s/eval-op s/op-14 [] nil)))
  (is (= [[(byte 15)]] (s/eval-op s/op-15 [] nil)))
  (is (= [[(byte 16)]] (s/eval-op s/op-16 [] nil))))

(deftest op-nop (is (= [] (s/eval-op s/op-nop [] nil))))

(deftest op-dup
  (is (= [(byte 1) (byte 1)] (s/eval-op s/op-dup [(byte 1)] nil))))

(deftest op-equalverify
  (letfn [(b [v] [(byte v)])]
    (is (= [()] (s/eval-op s/op-equal [(b 2) (b 3)] nil)))
    (is (= [(b 2)] (s/eval-op s/op-verify [(b 2) (b 3)] nil)))
    (is (= [(b 8)] (s/eval-op s/op-equalverify [(b 8) (b 2) (b 2)] nil)))
    (is (= ::s/halt (s/eval-op s/op-equalverify [(b 2) (b 3)] nil)))))

(deftest p2pk
  (let
    [z (biginteger
        0x7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d)
     pubkey-sec
     (e/hex->bytes
      "04887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34")
     sig-der
     (e/hex->bytes
      "3045022000eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c022100c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab601")
     script (s/->script [s/op-checksig pubkey-sec sig-der])]
    (is (s/evaluate script z))))

(deftest p2pk-fail
  (let
    [z (biginteger
        0x7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d)
     pubkey-sec
     (e/hex->bytes
      "04887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34")
     sig-der
     (e/hex->bytes
      "3045022000eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c022100c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab701")
     script (s/->script [s/op-checksig pubkey-sec sig-der])]
    (is (not (s/evaluate script z)))))

(deftest p2pkh
  (let
    [z (biginteger
        0x7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d)
     pubkey-sec
     (e/hex->bytes
      "04887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34")
     pubkey-hash (e/hex->bytes "fb6c931433c83e8bb5a4c6588c7fc24c08dac6e3")
     sig-der
     (e/hex->bytes
      "3045022000eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c022100c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab601")
     script (s/->script [s/op-checksig s/op-equalverify pubkey-hash s/op-hash160
                         s/op-dup pubkey-sec sig-der])]
    (is (s/evaluate script z))))

(deftest p2pkh-fail
  (let
    [z (biginteger
        0x7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d)
     pubkey-sec
     (e/hex->bytes
      "04887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34")
     pubkey-hash (e/hex->bytes "fb6c931433c83e8bb5a4c6588c7fc24c08dac6e4")
     sig-der
     (e/hex->bytes
      "3045022000eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c022100c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab601")
     script (s/->script [s/op-checksig s/op-equalverify pubkey-hash s/op-hash160
                         s/op-dup pubkey-sec sig-der])]
    (is (not (s/evaluate script z)))))
