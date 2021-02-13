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
     want1 "304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a71601"
     want2 "035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937"]
    (is (= want1 (e/bytes->hex (first (:cmds script)))))
    (is (= want2 (e/bytes->hex (second (:cmds script)))))))

(deftest serialize
  (let
    [want
     "6a47304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a7160121035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937"
     script (first (s/parse (e/hex->bytes want)))]
    (is (= want (e/bytes->hex (s/serialize script))))))
