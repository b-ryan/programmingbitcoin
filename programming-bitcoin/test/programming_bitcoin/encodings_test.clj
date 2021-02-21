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
    (is (= point (e/parse-sec (e/hex->bytes uncompressed))))
    (is (= point (e/parse-sec (e/hex->bytes compressed))))))

(deftest sec-2
  (let
    [point (ec/scalar-mul s256/G 123)
     uncompressed
     "04a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5204b5d6f84822c307e4b4a7140737aec23fc63b65b35f86a10026dbd2d864e6b"
     compressed
     "03a598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5"]
    (is (= uncompressed (e/hex (e/sec-uncompressed point))))
    (is (= compressed (e/hex (e/sec-compressed point))))
    (is (= point (e/parse-sec (e/hex->bytes uncompressed))))
    (is (= point (e/parse-sec (e/hex->bytes compressed))))))

(deftest sec-3
  (let
    [point (ec/scalar-mul s256/G 42424242)
     uncompressed
     "04aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e21ec53f40efac47ac1c5211b2123527e0e9b57ede790c4da1e72c91fb7da54a3"
     compressed
     "03aee2e7d843f7430097859e2bc603abcc3274ff8169c1a469fee0f20614066f8e"]
    (is (= uncompressed (e/hex (e/sec-uncompressed point))))
    (is (= compressed (e/hex (e/sec-compressed point))))
    (is (= point (e/parse-sec (e/hex->bytes uncompressed))))
    (is (= point (e/parse-sec (e/hex->bytes compressed))))))

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
         ["7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d"
          "9MA8fRQrT4u8Zj8ZRd6MAiiyaxb2Y1CMpvVkHQu5hVM6"]
         ["eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c"
          "4fE3H2E6XMp4SsxtwinF7w9a34ooUrwWe4WsW1458Pd"]
         ["c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6"
          "EQJsjkd6JaGwxrjEhfeqPenqHwrBmPQZjJGNSCHBkcF7"])]
    (doseq [[b16 b58] cases]
      (testing (str "b16->54 for " b16)
       (is (= b58 (e/bytes->base58 (e/hex->bytes b16))))
       (is (= b16
              (-> b16
                  e/hex->bytes
                  e/bytes->base58
                  e/base58->bytes
                  e/bytes->hex)))))))

(def sha256-test-vectors
  #^{:doc "test vectors from the NESSIE project (http://is.gd/jdM99e)"}
  [{:message ""
    :expected "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"}
   {:message "a"
    :expected "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb"}
   {:message "abc"
    :expected "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"}
   {:message "message digest"
    :expected "f7846f55cf23e14eebeab5b4e1550cad5b509e3348fbc4efa3a1413d393cb650"}
   {:message "abcdefghijklmnopqrstuvwxyz"
    :expected "71c480df93d6ae2f1efad1447c66c9525e316218cf51fc8d9ed832f2daf18b73"}
   {:message "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    :expected "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"}
   {:message "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    :expected "db4bfcbd4da0cd85a60c3c37d3fbd8805c77f15fc6b1fdfe614ee0a7c8fdb4c0"}])

(deftest sha256-test
  ;; Test the SHA256 implementation against the test vectors.
  (doseq [{:keys [message expected]} sha256-test-vectors]
    (testing (str "sha256 of \"" message "\"")
     (is (= expected (e/hex (e/sha256 (.getBytes message))))))))

(def hash256-test-vectors
  #^{:doc "test vectors from the NESSIE project (http://is.gd/jdM99e)"}
  [{:message ""
    :expected "5df6e0e2761359d30a8275058e299fcc0381534545f55cf43e41983f5d4c9456"}
   {:message "a"
    :expected "bf5d3affb73efd2ec6c36ad3112dd933efed63c4e1cbffcfa88e2759c144f2d8"}
   {:message "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    :expected "46b34a7719acaf5dd5054af0d1321063f7d09215ee16e128f694e809d2a82577"}])

(deftest sha256-test
  ;; Test the "hash256" implementation against the test vectors.
  (doseq [{:keys [message expected]} hash256-test-vectors]
    (testing (str "hash256 of \"" message "\"")
     (is (= expected (e/hex (e/sha256 (e/sha256 (.getBytes message))))))
     (is (= expected (e/hex (e/hash256 (.getBytes message))))))))

(deftest base58-with-checksum
  (let [cases
        (list
         ["7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d"
          "wdA2ffYs5cudrdkhFm5Ym94AuLvavacapuDBL2CAcvqYPkcvi"]
         ["eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c"
          "Qwj1mwXNifQmo5VV2s587usAy4QRUviQsBxoe4EJXyWz4GBs"]
         ["c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6"
          "2WhRyzK3iKFveq4hvQ3VR9uau26t6qZCMhADPAVMeMR6VraBbX"])]
    (doseq [[b16 b58] cases]
      (testing (str "b16->54 for " b16)
       (is (= b58 (e/base58-with-checksum (e/hex->bytes b16))))))))

(deftest hash160
  (let [cases (list
               ["" "3Wou4SXgm8Sq9fGKJAhAdn5d6uy4"]
               ["a" "38qrNdH3mVDxZWbURHef3EiaqPMU"]
               ["ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
                "2iLQ7bPqVzZpKMsP1gsdhvDcDsks"])]
    (doseq [[s b58] cases]
      (testing (str "hash160 for " s)
       (is (= b58 (e/bytes->base58 (e/hash160 (.getBytes s)))))))))

(defn- ** [x y] (.pow (biginteger x) (biginteger y)))

(deftest addresses
  (is (= "mmTPbXQFxboEtNRkwfh6K51jvdtHLxGeMA"
         (e/point->address (:point (s256/secret->private-key 5002))
                           {:compressed? false :testnet? true})))
  (is (= "mopVkxp8UhXqRYbCYJsbeE1h1fiF64jcoH"
         (e/point->address (:point (s256/secret->private-key (** 2020 5)))
                           {:compressed? true :testnet? true})))
  (is (= "1F1Pn2y6pDb68E5nYJJeba4TLg2U7B6KF1"
         (e/point->address (:point (s256/secret->private-key 0x12345deadbeef))
                           {:compressed? true :testnet? false}))))


(deftest wif
  (is (= "cMahea7zqjxrtgAbB7LSGbcQUr1uX1ojuat9jZodMN8rFTv2sfUK"
         (e/wif 5003 {:compressed? true :testnet? true})))
  (is (= "91avARGdfge8E4tZfYLoxeJ5sGBdNJQH4kvjpWAxgzczjbCwxic"
         (e/wif (** 2021 5) {:compressed? false :testnet? true})))
  (is (= "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgiuQJv1h8Ytr2S53a"
         (e/wif 0x54321deadbeef {:compressed? true :testnet? false}))))
