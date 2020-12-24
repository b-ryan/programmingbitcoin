(ns programming-bitcoin.finite-fields
  (:require [clojure.math.numeric-tower :refer [expt]]
            [clojure.spec.alpha :as s]
            [programming-bitcoin.primes :as primes]))

(require '[clojure.spec.gen.alpha :as gen])
(require '[clojure.spec.test.alpha :as stest])
(require '[clojure.test.check.generators :as cgen])

(defrecord Element [number prime])
(def e ->Element)

(s/def ::number nat-int?)
(s/def ::field-element
  (s/with-gen
   (s/and (s/keys :req-un [::number ::primes/prime])
          #(< (:number %) (:prime %)))
   #(cgen/let [prime (s/gen ::primes/prime)
               number (cgen/large-integer* {:min 0 :max (dec prime)})]
      (e number prime))))


(defn- mut
  "Returns a function that can be used to mutate the `Element` f1.

  The returned function will accept 1 argument, which is the new value for
  `:number`. This value will be `mod`ed by the `:prime` for the element."
  [{:keys [prime] :as f1}]
  #(e (int (mod % prime)) prime))

(s/def ::field-pair
  (s/with-gen (s/and (s/cat :f1 ::field-element
                            :f2 ::field-element)
                     #(= (:prime (:f1 %)) (:prime (:f2 %))))
              #(cgen/let [{:keys [prime] :as f1} (s/gen ::field-element)
                          f2-number (cgen/large-integer*
                                     {:min 0 :max (dec prime)})]
                 [f1 (e f2-number prime)])))

(def ^:private same-field
  #(= (:prime (:f1 (:args %))) (:prime (:ret %))))



(defn add [{n1 :number :as f1} {n2 :number}] ((mut f1) (+ n1 n2)))
(s/fdef add :args ::field-pair :ret ::field-element :fn same-field)
(stest/instrument `add)

#_(add (e 3 7) (e 6 7))



(defn sub [{n1 :number :as f1} {n2 :number}] ((mut f1) (- n1 n2)))
(s/fdef sub :args ::field-pair :ret ::field-element :fn same-field)
(stest/instrument `sub)

#_(sub (e 3 7) (e 6 7))



(defn mul [{n1 :number :as f1} {n2 :number}] ((mut f1) (* n1 n2)))
(s/fdef mul :args ::field-pair :ret ::field-element :fn same-field)
(stest/instrument `mul)

#_(mul (e 8 19) (e 17 19))



(defn pow
  [{n1 :number prime :prime :as f1} exponent]
  ((mut f1) (expt n1 (mod exponent (dec prime)))))
(s/fdef pow
  :args (s/cat :f1 ::field-element :exponent int?)
  :ret ::field-element
  :fn same-field)
(stest/instrument `pow)

#_(pow (e 0 1493) 1)



(defn div
  [{n1 :number prime :prime :as f1} {n2 :number}]
  ((mut f1) (* n1 (mod (expt n2 (- prime 2)) prime))))
(s/fdef div :args ::field-pair :ret ::field-element :fn same-field)
(stest/instrument `div)

#_(div (e 0 1493) (e 4 1493))



#_(do
    (prn "start")
    (prn (stest/check `div))
    (prn "end"))
