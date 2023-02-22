(ns cuid.modular-arithmetic
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [tupelo.math :as math]
    [tupelo.schema :as tsk]
    [tupelo.string :as str])
  (:import
    [java.util Random]))

(defn ceil-long [x] (long (Math/ceil (double x))))
(defn floor-long [x] (long (Math/floor (double x))))
(defn round-long [x] (long (Math/round (double x))))
(defn trunc-long [x] (long (.longValue (double x))))

(s/defn signum-long :- Long
  "Returns a Long value, either -1, 0, or +1, to indicate the sign of the input. "
  [x :- s/Num]
  (cond
    (pos? x) +1
    (neg? x) -1
    :else 0))

(s/defn same-sign :- s/Bool
  "Returns `true` iff x and y have the same sign, or are both zero."
  [x :- s/Num
   y :- s/Num]
  (truthy?
    (or
      (and (pos? x) (pos? y))
      (and (neg? x) (neg? y))
      (and (zero? x) (zero? y)))))

(defn div-mod
  "Given 2 args [n d], returns a 2-Vec [<quot> <mod>]"
  [n d]
  [(quot n d)
   (mod n d)])

;-----------------------------------------------------------------------------
; shortcuts

(s/defn mod-long :- Long
  [n :- Long
   d :- Long] (clojure.core/mod ^Long n ^Long d))

(s/defn quot-long :- Long
  [n :- Long
   d :- Long] (clojure.core/quot ^Long n ^Long d))

(s/defn mod-BigInteger :- BigInteger
  [n :- BigInteger
   d :- BigInteger] (.mod ^BigInteger n ^BigInteger d))

(s/defn quot-BigInteger :- BigInteger
  [n :- BigInteger
   d :- BigInteger] (.divide ^BigInteger n ^BigInteger d))

;-----------------------------------------------------------------------------
(s/defn add-mod-Long :- Long
  "Adds two numbers a and b (mod N)."
  [a :- Long
   b :- Long
   N :- Long]
  (assert (and (pos? N) (< 1 N)))
  (it-> (+ a b)
    (mod it N)))

(s/defn mult-mod-Long :- Long
  "Multiply two numbers a and b (mod N)."
  [a :- Long
   b :- Long
   N :- Long]
  (assert (and (pos? N) (< 1 N)))
  (it-> (* a b)
    (mod it N)))

(s/defn add-mod-BigInteger :- BigInteger
  "Adds two numbers a and b (mod N)."
  [a :- BigInteger
   b :- BigInteger
   N :- BigInteger]
  (assert (and (pos? N) (< 1 N)))
  (it-> (.add ^BigInteger  a ^BigInteger b)
    (mod-BigInteger it N)))

(s/defn mult-mod-BigInteger :- BigInteger
  "Multiply two numbers a and b (mod N)."
  [a :- BigInteger
   b :- BigInteger
   N :- BigInteger]
  (assert (and (pos? N) (< 1 N)))
  (it-> (.multiply ^BigInteger a ^BigInteger b)
    (mod-BigInteger it N)))

;-----------------------------------------------------------------------------
(s/defn mod-symmetric :- s/Int
  "Like clojure.core/mod, but returns a result symmetric around zero [-D/2..D/2). D must be even and positive."
  [numer :- s/Int
   D :- s/Int]
  (assert (and (int? numer) (int-pos? D) (even? D)))
  (let [d-ovr-2 (/ D 2)
        result  (cond-it-> (clojure.core/mod numer D)
                  (<= d-ovr-2 it) (- it D))]
    result))

(defn modInverse
  "Computes the 'inverse` y of a number x (mod N), such that `x*y (mod N)` = 1.
  Uses the extended Euclid algorithm (iterative version). Assumes x and N are relatively prime. "
  [x N]
  (assert (and (pos? x) (pos? N) (< x N)))
  (let [N-orig N
        a      1
        b      0]
    (if (= 1 N)
      (throw (ex-info "Invalid N" (vals->map x N)))
      (loop [x x
             n N
             a a
             b b]
        (if (< 1 x)
          (let [x-next n
                N-next (mod x n)
                q      (quot x n)
                a-next b
                b-next (- a (* q b))]
            (recur x-next N-next a-next b-next))
          (if (neg? a)
            (+ a N-orig)
            a))))))


