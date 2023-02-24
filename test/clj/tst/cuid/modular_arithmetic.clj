(ns tst.cuid.modular-arithmetic
  (:use cuid.modular-arithmetic
        tupelo.core
        tupelo.test)
  (:require
    [com.climate.claypoole :as cp]
    [criterium.core :as crit]
    [tupelo.math :as math]
    [tupelo.profile :as prof]
    [tupelo.schema :as tsk]
    ))

(set! *warn-on-reflection* true)

(def verbose? false) ; <= enable to see extra printouts

(def bi-3 (biginteger 3))
(def bi-10 (biginteger 10))

; does even? work for BigInteger values?
(verify
  ; even? works for both
  (isnt (even? 1))
  (is (even? 2))
  (isnt (even? bi-3))
  (is (even? bi-10))

  ; What is the return type for quot/mod re Long vs BigInteger?
  (is= Long (type (quot 10 3)))
  (is= Long (type (mod 10 3)))
  (is= clojure.lang.BigInt (type (quot bi-10 bi-3)))
  (is= clojure.lang.BigInt (type (mod bi-10 bi-3)))
  (is= java.math.BigInteger (type (quot-BigInteger bi-10 bi-3)))
  (is= java.math.BigInteger (type (mod-BigInteger bi-10 bi-3)))
  )

(verify
  (is= 5 (ceil-long 4.5))
  (is= 4 (floor-long 4.5))

  (is= 5 (round-long 4.6))
  (is= 5 (round-long 4.5))
  (is= 4 (round-long 4.4))

  (is= 4 (trunc-long 4.4))
  (is= -4 (trunc-long -4.4))

  (is= 1 (signum-long 4.4))
  (is= 0 (signum-long 0))
  (is= -1 (signum-long -4.4))

  (is (same-sign 1 1))
  (is (same-sign -1 -1))
  (isnt (same-sign 1 -1))
  (isnt (same-sign -1 1)))

(verify
  (is= (forv [i (thru -5 5)]
         (quot i 3))
    [-1 -1 -1 0 0 0 0 0 1 1 1])
  (is= (forv [i (thru -5 5)]
         (mod i 3))
    [1 2 0 1 2 0 1 2 0 1 2])

  (let [res (mod-Long 10 3)]
    (is= 1 res)
    (is= Long (type res)))
  (let [res (mod-BigInteger (biginteger 10) (biginteger 3))]
    (is= 1 res)
    (is= BigInteger (type res)))

  (let [res (quot-Long 10 3)]
    (is= 3 res)
    (is= Long (type res)))
  (let [res (quot-BigInteger (biginteger 10) (biginteger 3))]
    (is= 3 res)
    (is= BigInteger (type res)))

  ; BigInteger.mod() will always return a positive value
  (let [bi-10     (biginteger 10)
        bi-10-neg (biginteger 10)
        bi-15 (biginteger 15)
        bi-15-neg (biginteger -15)]
    (is= 5 (mod-BigInteger bi-15 bi-10))
    (is= 5 (mod-BigInteger bi-15-neg bi-10))
    (is= 5 (mod-BigInteger bi-15 bi-10-neg))
    (is= 5 (mod-BigInteger bi-15-neg bi-10-neg))))

(verify
  (throws? (add-mod-Long 7 9 1))
  (throws? (add-mod-Long 7 9 -5))
  (is= 1 (add-mod-Long 7 9 5))

  (throws? (mult-mod-Long 7 9 1))
  (throws? (mult-mod-Long 7 9 -5))
  (is= 3 (mult-mod-Long 7 9 5))

  ; Note that Clojure can compare (long <n>) and (biginteger <n>)
  (let [bi-1 (biginteger 1)
        bi-5neg (biginteger -5)
        bi-5 (biginteger 5)
        bi-7 (biginteger 7)
        bi-9 (biginteger 9)]
  (throws? (add-mod-BigInteger bi-7 bi-9 bi-1))
  (throws? (add-mod-BigInteger bi-7 bi-9 bi-5neg))
  (is= 1 (add-mod-BigInteger   bi-7 bi-9 bi-5))

  (throws? (mult-mod-BigInteger bi-7 bi-9 bi-1))
  (throws? (mult-mod-BigInteger bi-7 bi-9 bi-5neg))
  (is= 3   (mult-mod-BigInteger bi-7 bi-9 bi-5))))

;-----------------------------------------------------------------------------
(verify
  (throws? (mod-symmetric 5.1 16))
  (throws? (mod-symmetric 5 17))
  (throws? (mod-symmetric 5 16.1))
  (throws? (mod-symmetric 5 -16))

  (is= (forv [n (thru -10 10)]
         (mod-symmetric n 16))
    [6 7 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 -8 -7 -6]))

;-----------------------------------------------------------------------------
(verify
  (let [verify-inv (fn verify-inv-fn
                     [x N]
                     (let [xinv   (modInverse x N)
                           result (mult-mod-Long x xinv N)]
                       (is= 1 result)
                       (vals->map x xinv N)))]
    (modInverse 3 11)
    (verify-inv 3 11)
    (verify-inv 3 16)
    (verify-inv 5 16)
    (verify-inv 7 16)
    (verify-inv 9 16)

    (doseq [i (range 3 32 2)]
      (let [result (verify-inv i 32)]
        (when verbose?
          (spyx result))))))


