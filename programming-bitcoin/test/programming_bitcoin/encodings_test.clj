(ns programming-bitcoin.encodings-test
  (:require [clojure.test :refer [deftest testing is]]
            [programming-bitcoin.secp256k1 :as s256]
            [programming-bitcoin.elliptic-curves :as ec]
            [programming-bitcoin.encodings :as e]))

(deftest sec-1
  (let
    [point (ec/scalar-mul s256/G (.pow (biginteger 999) (biginteger 3)))
     uncompressed
     "049d5ca49670cbe4c3bfa84c96a8c87df086c6ea6a24ba6b809c9de234496808d56fa15cc7f3d38cda98dee2419f415b7513dde1301f8643cd9245aea7f3f911f9"
     compressed
     "039d5ca49670cbe4c3bfa84c96a8c87df086c6ea6a24ba6b809c9de234496808d5"]
    (is (= uncompressed (e/sec-uncompressed point)))
    (is (= compressed (e/sec-compressed point)))
    (is (= point (e/parse-sec uncompressed)))
    (is (= point (e/parse-sec compressed)))))

(deftest sec-2
  (let
    [point (ec/scalar-mul s256/G 123)
     uncompressed
     "04a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5204b5d6f84822c307e4b4a7140737aec23fc63b65b35f86a10026dbd2d864e6b"
     compressed
     "03a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5"]
    (is (= uncompressed (e/sec-uncompressed point)))
    (is (= compressed (e/sec-compressed point)))
    (is (= point (e/parse-sec uncompressed)))
    (is (= point (e/parse-sec compressed)))))

(deftest sec-3
  (let
    [point (ec/scalar-mul s256/G 42424242)
     uncompressed
     "04aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e21ec53f40efac47ac1c5211b2123527e0e9b57ede790c4da1e72c91fb7da54a3"
     compressed
     "03aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e"]
    (is (= uncompressed (e/sec-uncompressed point)))
    (is (= compressed (e/sec-compressed point)))
    (is (= point (e/parse-sec uncompressed)))
    (is (= point (e/parse-sec compressed)))))
