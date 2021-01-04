(ns programming-bitcoin.encodings-test
  (:require [clojure.test :refer [deftest testing is]]
            [programming-bitcoin.secp256k1 :as s256]
            [programming-bitcoin.elliptic-curves :as ec]
            [programming-bitcoin.encodings :as e]
            [programming-bitcoin.primitives :refer [rand-biginteger]]))

(deftest sec-1
  (let
    [point (ec/scalar-mul s256/G (.pow (biginteger 999) (biginteger 3)))
     uncompressed
     "049d5ca49670cbe4c3bfa84c96a8c87df086c6ea6a24ba6b809c9de234496808d56fa15cc7f3d38cda98dee2419f415b7513dde1301f8643cd9245aea7f3f911f9"
     compressed
     "039d5ca49670cbe4c3bfa84c96a8c87df086c6ea6a24ba6b809c9de234496808d5"]
    (is (= uncompressed (e/hex (e/sec-uncompressed point))))
    (is (= compressed (e/hex (e/sec-compressed point))))
    (is (= point (e/parse-sec uncompressed)))
    (is (= point (e/parse-sec compressed)))))

(deftest sec-2
  (let
    [point (ec/scalar-mul s256/G 123)
     uncompressed
     "04a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5204b5d6f84822c307e4b4a7140737aec23fc63b65b35f86a10026dbd2d864e6b"
     compressed
     "03a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5"]
    (is (= uncompressed (e/hex (e/sec-uncompressed point))))
    (is (= compressed (e/hex (e/sec-compressed point))))
    (is (= point (e/parse-sec uncompressed)))
    (is (= point (e/parse-sec compressed)))))

(deftest sec-3
  (let
    [point (ec/scalar-mul s256/G 42424242)
     uncompressed
     "04aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e21ec53f40efac47ac1c5211b2123527e0e9b57ede790c4da1e72c91fb7da54a3"
     compressed
     "03aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e"]
    (is (= uncompressed (e/hex (e/sec-uncompressed point))))
    (is (= compressed (e/hex (e/sec-compressed point))))
    (is (= point (e/parse-sec uncompressed)))
    (is (= point (e/parse-sec compressed)))))

(deftest der
  (let [cases (list [1 2]
                    [(rand-biginteger (.pow (biginteger 2) (biginteger 256)))
                     (rand-biginteger (.pow (biginteger 2) (biginteger 255)))])]
    (doseq [[r s] cases
            :let [sig (s256/->sig r s)]]
      (testing (format "DER sig for %s" sig)
       (is (= sig (e/parse-der (e/der sig))))))))

(deftest base58
  (let [cases
        (list
         ["9MA8fRQrT4u8Zj8ZRd6MAiiyaxb2Y1CMpvVkHQu5hVM6"
          "7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d"]
         ["4fE3H2E6XMp4SsxtwinF7w9a34ooUrwWe4WsW1458Pd"
          "eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c"]
         ["EQJsjkd6JaGwxrjEhfeqPenqHwrBmPQZjJGNSCHBkcF7"
          "c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6"])]
    (doseq [[b58 b16] cases]
      (testing (str "b16->54 for " b16)
       (is (= b58 (e/base58 (e/hex->bytes b16))))))))
