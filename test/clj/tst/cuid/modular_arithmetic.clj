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

  (let [res (mod-long 10 3)]
    (is= 1 res)
    (is= Long (type res)))
  (let [res (mod-BigInteger (biginteger 10) (biginteger 3))]
    (is= 1 res)
    (is= BigInteger (type res)))

  (let [res (quot-long 10 3)]
    (is= 3 res)
    (is= Long (type res)))
  (let [res (quot-BigInteger (biginteger 10) (biginteger 3))]
    (is= 3 res)
    (is= BigInteger (type res)))

  ; BigInteger.mod() will always return a positive value
  (let [bi-10     (biginteger 10)
        bi-10-neg (biginteger 10)]
    (is= 5 (mod-BigInteger (biginteger 15) bi-10))
    (is= 5 (mod-BigInteger (biginteger -15) bi-10))
    (is= 5 (mod-BigInteger (biginteger 15) bi-10-neg))
    (is= 5 (mod-BigInteger (biginteger -15) bi-10-neg)))


  (is= [3 1] (div-mod 10 3))
  ; vvv Given the below results, probably best not to use for any negative args
  (is= [-3 2] (div-mod -10 3))
  (is= [-3 -2] (div-mod 10 -3))
  (is= [3 -1] (div-mod -10 -3)))

(verify
  (throws? (add-mod-Long 7 9 1))
  (throws? (add-mod-Long 7 9 -5))
  (is= 1 (add-mod-Long 7 9 5))

  (throws? (mult-mod-Long 7 9 1))
  (throws? (mult-mod-Long 7 9 -5))
  (is= 3 (mult-mod-Long 7 9 5))

  ; Note that Clojure can compare (long <n>) and (biginteger <n>)
  (throws? (add-mod-BigInteger (biginteger 7) (biginteger 9) (biginteger 1)))
  (throws? (add-mod-BigInteger (biginteger 7) (biginteger 9) (biginteger -5)))
  (is= 1 (add-mod-BigInteger (biginteger 7) (biginteger 9) (biginteger 5)))

  (throws? (mult-mod-BigInteger (biginteger 7) (biginteger 9) (biginteger 1)))
  (throws? (mult-mod-BigInteger (biginteger 7) (biginteger 9) (biginteger -5)))
  (is= 3 (mult-mod-BigInteger (biginteger 7) (biginteger 9) (biginteger 5))))

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


