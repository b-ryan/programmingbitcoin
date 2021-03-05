(ns programming-bitcoin.secp256k1-test
  (:require [clojure.test :refer [deftest testing is]]
            [programming-bitcoin.secp256k1 :as s256]
            [programming-bitcoin.elliptic-curves :as ec]
            [programming-bitcoin.primitives :refer [rand-biginteger]]))

(defn- ** [x y] (.pow (biginteger x) (biginteger y)))

(deftest pub-point
  (let [points
        (list
         [7 0x5cbdf0646e5db4eaa398f365f2ea7a0e3d419b7e0330e39ce92bddedcac4f9bc
          0x6aebca40ba255960a3178d6d861a54dba813d0b813fde7b5a5082628087264da]
         [1485
          0xc982196a7466fbbbb0e27a940b6af926c1a74d5ad07128c82824a11b5398afda
          0x7a91f9eae64438afb9ce6448a1c133db2d8fb9254e4546b6f001637d50901f55]
         [(** 2 128)
          0x8f68b9d2f63b5f339239c1ad981f162ee88c5678723ea3351b7b444c9ec4c0da
          0x662a9f2dba063986de1d90c2b6be215dbbea2cfe95510bfdf23cbf79501fff82]
         [(.add (** 2 240) (** 2 31))
          0x9577ff57c8234558f293df502ca4f09cbc65a6572c842b39b366f21717945116
          0x10b49c67fa9365ad7b90dab070be339a1daf9052373ec30ffae4f72d5e66d053])]
    (doseq [[secret x y] points]
      (is (= (s256/p x y) (ec/scalar-mul s256/G secret))))))

(deftest signature-verification
  (let [point
        (s256/p
         0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c
         0x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34)
        z (biginteger
           0xec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60)
        r (biginteger
           0xac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395)
        s (biginteger
           0x68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4)
        sig (s256/->Signature r s)]
    (is (s256/valid-signature? sig point z))))

(deftest signature-verification-2
  (let [point
        (s256/p
         0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c
         0x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34)
        z (biginteger
           0x7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d)
        r (biginteger
           0xeff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c)
        s (biginteger
           0xc7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6)
        sig (s256/->Signature r s)]
    (is (s256/valid-signature? sig point z))))

(deftest sign
  (let [pk (s256/secret->private-key (s256/rand-secret))
        z (rand-biginteger (.pow (biginteger 2) (biginteger 256)))
        sig (s256/sign pk z)]
    (is (s256/valid-signature? sig (:point pk) z))))
