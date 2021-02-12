(ns programming-bitcoin.transactions-test
  (:require [clojure.test :refer [deftest testing is]]
            [programming-bitcoin.encodings :as e]
            [programming-bitcoin.transactions :as tx]))

(defn- read-test-tx
  [fname]
  (-> fname
      clojure.java.io/resource
      slurp
      (clojure.string/replace #"\n" "")
      ))

(deftest parse
  (is (= 4 (tx/parse (e/hex->bytes (read-test-tx "tx1.txt"))))))
