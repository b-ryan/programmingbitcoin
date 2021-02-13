(ns programming-bitcoin.transactions-test
  (:require [clojure.test :refer [deftest testing is]]
            [programming-bitcoin.encodings :as e]
            [programming-bitcoin.script :as script]
            [programming-bitcoin.transactions :as tx]))

(def tx1
  (e/hex->bytes
   "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600"))

(deftest parse
  (let [parsed (tx/parse tx1)
        in-1 (first (:tx-ins parsed))
        [out-1 out-2] (:tx-outs parsed)]
    (testing "version" (is (= 1 (:version parsed))))
    (testing "inputs"
     (is (= 1 (count (:tx-ins parsed))))
     (is (= "d1c789a9c60383bf715f3f6ad9d14b91fe55f3deb369fe5d9280cb1a01793f81"
            (e/bytes->hex (:prev-tx in-1)))))
    (is (= 0 (:prev-index in-1)))
    (is
     (=
      "6b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"
      (-> in-1
          :script-sig
          script/serialize
          e/bytes->hex)))
    (is (= 0xfffffffe (:sequence* in-1)))
    (testing "outputs"
     (is (= 2 (count (:tx-outs parsed))))
     (is (= 32454049 (:amount out-1)))
     (is (= 10011545 (:amount out-2)))
     (is (= "1976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac"
            (-> out-1
                :script-pubkey
                script/serialize
                e/bytes->hex)))
     (is (= "1976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac"
            (-> out-2
                :script-pubkey
                script/serialize
                e/bytes->hex))))
    (testing "locktime"
      (is (= 410393 (:locktime parsed))))))
