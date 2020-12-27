(ns programming-bitcoin.signatures
  (:require [programming-bitcoin.elliptic-curves :as ec]))

(defrecord Signature [r s])

(defn verify
  [z {:keys [r s] :as signature}]
  (let [s-inv (.pow ^BigInteger s )])
  )

#_(let [p (programming-bitcoin.bitcoin-curve/point
           0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c
           0x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34)
        z (biginteger 0xec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60)
        r (biginteger 0xac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395)
        s (biginteger 0x68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4)
        sig (->Signature r s)]
    (verify z sig)
    )
