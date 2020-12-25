(ns programming-bitcoin.elliptic-curves
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.math.numeric-tower :refer [expt]]
            [clojure.spec.alpha :as s]
            [programming-bitcoin.primitives :refer [Primitive] :as prim]))

(require '[clojure.test.check.generators :as cgen])

(def ^:private + prim/add)
(def ^:private - prim/sub)
(def ^:private * prim/mul)
(def ^:private / prim/div)
(def ^:private ** prim/pow)
(def ^:private s* prim/scalar-mul)

(defrecord Point [x y a b])
(def p ->Point)

(s/def ::x ::prim/primitive)
(s/def ::y ::prim/primitive)
(s/def ::a ::prim/primitive)
(s/def ::b ::prim/primitive)

(defn- inf? [{:keys [x y]}] (= [x y] [:inf :inf]))

(defn- on-curve?
  [{:keys [x y a b] :as point}]
  (or (inf? point)
      (= (** y 2)
         (-> (** x 3)
             (+ (* a x))
             (+ b)))))

(s/def ::point
  (s/with-gen (s/and (s/keys :req-un [::x ::y ::a ::b])
                     on-curve?)
              #(cgen/let [x (s/gen int?)
                          y (s/gen int?)
                          a (s/gen int?)
                          b (s/gen int?)]
                 (p x y a b))))

(defn- same-curve?
  [p1 p2]
  (= ((juxt :a :b) p1) ((juxt :a :b) p2)))

(defn- same-curve?-spec
  [p1-fn p2-fn]
  #(same-curve? (p1-fn %) (p2-fn %)))

(s/def ::point-pair
  (s/with-gen (s/and (s/cat :p1 ::point
                            :p2 ::point)
                     (same-curve?-spec :p1 :p2))
              #(cgen/let [{:keys [a b] :as p1} (s/gen ::point)
                          p2-x (s/gen int?)
                          p2-y (s/gen int?)]
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
    (and (= x1 x2) (= y1 y2))
    (let [slope (-> (** x1 2)
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
    (= x1 x2) (p :inf :inf a b)
    ;; Else:
    ;; Formula (x3,y3)==(x1,y1)+(x2,y2)
    ;; s=(y2-y1)/(x2-x1)
    ;; x3=s**2-x1-x2
    ;; y3=s*(x1-x3)-y1
    :else
    (let [slope (-> (- y2 y1)
                    (/ (- x2 x1)))
          x3 (-> (** slope 2)
                 (- x1)
                 (- x2))
          y3 (-> (- x1 x3)
                 (* slope)
                 (- y1))]
      (p x3 y3 a b))))

(s/fdef add
  :args ::point-pair
  :ret ::point
  :fn (same-curve?-spec (comp :p1 :args) :ret))

(defn mul
  [p1 p2])

(defn pow
  [p1 p2])
