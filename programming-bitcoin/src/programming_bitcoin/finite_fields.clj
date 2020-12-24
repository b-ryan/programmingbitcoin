(ns programming-bitcoin.finite-fields
  (:require [clojure.math.numeric-tower :refer [expt]]
            [clojure.spec.alpha :as s]))

(require '[clojure.spec.gen.alpha :as gen])
(require '[clojure.spec.test.alpha :as stest])
(require '[clojure.test.check.generators :as cgen])

(defn prime?
  [n]
  (let [is-factor? (comp zero? (partial mod n))
        square #(* % %)]
    (cond (<= n 3) (> n 1)
          (or (is-factor? 2) (is-factor? 3)) false
          :else
          (->> (iterate (partial + 6) 5)
               (map #(cond
                       (> (square %) n) true
                       (or (is-factor? %) (is-factor? (+ % 2))) false))
               (drop-while nil?)
               first))))

(def first-primes
  (->> (-> "primes.txt"
           clojure.java.io/resource
           slurp
           clojure.string/split-lines)
       (map #(Integer/parseInt %))
       (set)))

(s/def ::prime
  (s/with-gen (s/and integer?
                     pos?
                     prime?)
              #(s/gen first-primes)))

(defrecord FieldElement [number prime])

(s/def ::number nat-int?)
(s/def ::field-element
  (s/with-gen
   (s/and (s/keys :req-un [::number ::prime])
          #(< (:number %) (:prime %)))
   #(cgen/let [prime (s/gen ::prime)
               number (cgen/large-integer* {:min 0, :max (dec prime)})]
      (->FieldElement number prime))))

(defn add
  [{n1 :number, prime :prime} {n2 :number}]
  (->FieldElement (mod (+ n1 n2) prime) prime))

(s/def ::field-pair
  (s/with-gen (s/and (s/cat :f1 ::field-element
                            :f2 ::field-element)
                     #(= (:prime (:f1 %)) (:prime (:f2 %))))
              #(cgen/let [{:keys [prime], :as f1} (s/gen ::field-element)
                          f2-number (cgen/large-integer*
                                     {:min 0, :max (dec prime)})]
                 [f1 (->FieldElement f2-number prime)])))

(def ^:private first-elem-ret-same-prime
  #(= (:prime (:f1 (:args %))) (:prime (:ret %))))

(s/fdef add
  :args ::field-pair
  :ret ::field-element
  :fn first-elem-ret-same-prime)

(stest/instrument `add)

#_(add (->FieldElement 3 7) (->FieldElement 6 7))



(defn mul
  [{n1 :number, prime :prime} {n2 :number}]
  (->FieldElement (mod (* n1 n2) prime) prime))

(s/fdef mul
  :args ::field-pair
  :ret ::field-element
  :fn first-elem-ret-same-prime)

(stest/instrument `mul)

#_(mul (->FieldElement 8 19) (->FieldElement 17 19))



(defn pow
  [{n1 :number, prime :prime} exponent]
  (->FieldElement (int (mod (expt n1 exponent) prime)) prime))

(s/fdef pow
  :args (s/cat :f1 ::field-element
               :exponent (s/with-gen nat-int?
                                     #(cgen/large-integer* {:min 0, :max 100})))
  :ret ::field-element
  :fn first-elem-ret-same-prime)

(stest/instrument `pow)

#_(pow (->FieldElement 0 1493) 1)




#_(do
    (prn "start")
    (prn (stest/check `pow))
    (prn "end"))
