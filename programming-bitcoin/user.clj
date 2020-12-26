(ns user)


(defmacro spy
  [x]
  `(let [v# ~x]
     (print (format "\n****** SPY\n  %s\n  => " '~x))
     (clojure.pprint/pprint v#)
     v#))

(defmacro capture
  [sym & body]
  `(let [result# (do ~@body)]
     (def ~sym result#)
     result#))

; (require '[clojure.spec.test.alpha :as stest])
; (stest/instrument (stest/instrumentable-syms))
; (stest/instrument 'programming-bitcoin.finite-fields/pow)
