(ns programming-bitcoin.elliptic-curves
  (:refer-clojure :exclude [+ - * / &])
  (:require [clojure.spec.alpha :as s]
            [programming-bitcoin.primitives :as prim]))

; (require '[clojure.test.check.generators :as cgen])
; (require '[clojure.spec.test.alpha :as stest])

(def ^:private + #'prim/add)
(def ^:private - #'prim/sub)
(def ^:private * #'prim/mul)
(def ^:private / #'prim/div)
(def ^:private ** #'prim/pow)
(def ^:private s* #'prim/scalar-mul)

(defrecord Point [x y a b])
(def ^{:doc "Creates a `Point`"} p ->Point)

(defn- bi-or-inf [v] (if (= v :inf) v (biginteger v)))

(defn bi-p
  "Makes a point from bigintegers"
  [x y a b]
  (p (bi-or-inf x) (bi-or-inf y) (biginteger a) (biginteger b)))

#_(s/def ::x ::prim/primitive)
#_(s/def ::y ::prim/primitive)
#_(s/def ::a ::prim/primitive)
#_(s/def ::b ::prim/primitive)

(defn inf [a b] (p :inf :inf a b))
(defn inf? [{:keys [x y]}] (= [x y] [:inf :inf]))

(defn- on-curve?
  [{:keys [x y a b] :as point}]
  (or (inf? point)
      (= (** y 2)
         (-> (** x 3)
             (+ (* a x))
             (+ b)))))

#_(s/def ::point
    (s/with-gen (s/and (s/keys :req-un [::x ::y ::a ::b]) on-curve?)
                #(cgen/let [x (s/gen integer?)
                            y (s/gen integer?)
                            a (s/gen integer?)
                            b (s/gen integer?)]
                   (p x y a b))))

(s/def ::point (s/and (s/keys :req-un [::x ::y ::a ::b]) on-curve?))

(defn- same-curve? [p1 p2] (= ((juxt :a :b) p1) ((juxt :a :b) p2)))

(defn- same-curve?-spec [p1-fn p2-fn] #(same-curve? (p1-fn %) (p2-fn %)))

(def ^:private same-curve?-fn-spec (same-curve?-spec (comp :p1 :args) :ret))

#_(s/def ::point-pair
    (s/with-gen (s/and (s/cat :p1 ::point
                              :p2 ::point)
                       (same-curve?-spec :p1 :p2))
                #(cgen/let [{:keys [a b] :as p1} (s/gen ::point)
                            p2-x (s/gen integer?)
                            p2-y (s/gen integer?)]
                   [p1 (p p2-x p2-y a b)])))

(defn add
  [{x1 :x y1 :y a :a b :b :as p1} {x2 :x y2 :y :as p2}]
  (cond
    (inf? p1) p2
    (inf? p2) p1
    ;; Case 1: self == other
    ;; Formula (x3,y3)=(x1,y1)+(x1,y1)
    ;; s=(3*x1**2+a)/(2*y1)
    ;; x3=s**2-2*x1
    ;; y3=s*(x1-x3)-y1
    (and (= x1 x2) (= y1 y2)) (let [slope (-> (** x1 2)
                                              (s* 3)
                                              (+ a)
                                              (/ (s* y1 2)))
                                    x3 (-> (** slope 2)
                                           (- (s* x1 2)))
                                    y3 (-> (- x1 x3)
                                           (* slope)
                                           (- y1))]
                                (p x3 y3 a b))
    ;; Case 2: self.x == other.x, self.y != other.y
    ;; Result is point at infinity
    (= x1 x2) (inf a b)
    ;; Else:
    ;; Formula (x3,y3)==(x1,y1)+(x2,y2)
    ;; s=(y2-y1)/(x2-x1)
    ;; x3=s**2-x1-x2
    ;; y3=s*(x1-x3)-y1
    :else (let [slope (-> (- y2 y1)
                          (/ (- x2 x1)))
                x3 (-> (** slope 2)
                       (- x1)
                       (- x2))
                y3 (-> (- x1 x3)
                       (* slope)
                       (- y1))]
            (p x3 y3 a b))))

#_(s/fdef add :args ::point-pair :ret ::point :fn same-curve?-fn-spec)



(defn- >>
  [x n]
  (if (instance? BigInteger x)
    (.shiftRight x (BigInteger/valueOf n))
    (bit-shift-right x n)))

(defn- &
  [x n]
  (if (instance? BigInteger x) (.and x (BigInteger/valueOf n)) (bit-and x n)))

(defn scalar-mul
  [{:keys [x y a b] :as p1} coefficient]
  {:pre [(integer? coefficient)]}
  (->> (iterate #(>> % 1) coefficient)
       (take-while (partial < 0))
       (reduce (fn [[result current] coeff] [(if (> (& coeff 1) 0)
                                               (add result current)
                                               result) (add current current)])
               [(inf a b) p1])
       (first)))

#_(->> (iterate #(>> % 1) 1)
       (take-while (partial < 0)))

#_(s/fdef scalar-mul
    :args (s/cat :p1 ::point
                 :coefficient integer?)
    :ret ::point
    :fn same-curve?-fn-spec)


;; coeff x1 y1 x2 y2
;; (192 105) * 2 == (49, 71)
#_(let [prime 223
        e programming-bitcoin.finite-fields/e
        a (e 0 prime)
        b (e 7 prime)
        x (e 192 prime)
        y (e 105 prime)
        p1 (p x y a b)]
    (clojure.pprint/pprint (add p1 p1))
    (clojure.pprint/pprint (scalar-mul p1 2))
    (println "------------------------------------"))
