(ns tst.cuid.core
  (:use cuid.core
        tupelo.core
        tupelo.test)
  (:require
    [com.climate.claypoole :as cp]
    [criterium.core :as crit]
    [schema.core :as s]
    [tupelo.math :as math]
    [tupelo.profile :as prof]
    [tupelo.schema :as tsk]
    ))

(set! *warn-on-reflection* true)

(def visual-debugging? false) ; <= enable to see extra printouts

; does even? work for BigInteger values?
(verify
  (isnt (even? (biginteger 1)))
  (is (even? (biginteger 2))))

(verify
  (let [bi-five (biginteger 5)]
    ; How does it cost us to cast to BigInteger?
    (while false
      (nl) (prn :5)
      (crit/quick-bench (biginteger 5)) ;              Long:  7 nanosec
      (nl) (prn :bi-five)
      (crit/quick-bench (biginteger bi-five))) ; BigINteger:  4 nanosec

    ; ensure s/validate does what we want
    (throws? (s/validate BigInteger 5))
    (s/validate BigInteger bi-five)
    (s/validate s/Int bi-five)
    (s/validate s/Int 5)

    ; Make sure it works correctly
    (throws? (int->bitstr 5 2))
    (is= "101" (int->bitstr 5 3))
    (is= "0101" (int->bitstr 5 4))
    (is= "00000101" (int->bitstr 5 8))))

(verify
  ; BigInteger.mod() will always return a positive value
  (let [bi-10     (biginteger 10)
        bi-10-neg (biginteger 10)]
    (is= 5 (.mod (biginteger 15) bi-10))
    (is= 5 (.mod (biginteger -15) bi-10))
    (is= 5 (.mod (biginteger 15) bi-10-neg))
    (is= 5 (.mod (biginteger -15) bi-10-neg))))

(verify
  (throws? (mod-symmetric 5.1 16))
  (throws? (mod-symmetric 5 17))
  (throws? (mod-symmetric 5 16.1))
  (throws? (mod-symmetric 5 -16))
  (is= (forv [n (thru -10 10)]
         (mod-symmetric n 16))
    [6 7 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 -8 -7 -6]))

(verify
  (let [N 16
        m 3]
    (is= (forv [i (thru -5 5)]
           (quot i 3))
      [-1 -1 -1 0 0 0 0 0 1 1 1])
    (is= (forv [i (thru -5 5)]
           (round-long (/ i 3)))
      [-2 -1 -1 -1 0 0 0 1 1 1 2])
    (is= (forv [i (thru -5 5)]
           (mod i 3))
      [1 2 0 1 2 0 1 2 0 1 2])))

(verify
  (throws? (add-mod 4 3 1))
  (throws? (add-mod 4 3 -5))
  (is= 2 (add-mod 4 3 5))

  (throws? (mult-mod 4 3 1))
  (throws? (mult-mod 4 3 -5))
  (is= 2 (mult-mod 4 3 5))

  )



(defn verify-inv
  [x N]
  (let [xinv   (modInverse x N)
        result (mult-mod x xinv N)]
    (is= 1 result)
    (vals->map x xinv N)))

(verify
  (spyx (modInverse 3 11))
  (spyx (verify-inv 3 11))

  (nl)
  (spyx (verify-inv 3 16))
  (spyx (verify-inv 5 16))
  (spyx (verify-inv 7 16))
  (spyx (verify-inv 9 16))

  (nl)
  (doseq [i (range 3 32 2)]
    (spyx (verify-inv i 32)))

  (prn :-----------------------------------------------------------------------------)
  (let [N 32]
    (doseq [m (range 1 16 2)]
      (nl)
      (prn (vals->map m))
      (doseq [x (range 16)]
        (let [c      (crypt N m x)
              result (decrypt N m c)]
          (prn (vals->map x c result))
          (is= x result)
          ))))

  )



(comment
  ;-----------------------------------------------------------------------------
  (verify
    (is= [:b :c :d :a] (vec-shuffle
                         [[0 1]
                          [1 2]
                          [2 3]
                          [3 0]]
                         [:a :b :c :d]))
    (is= [:d :c :b :a] (vec-shuffle
                         [[0 3]
                          [1 2]
                          [2 1]
                          [3 0]]
                         [:a :b :c :d]))
    (is= [:c :b :d :a] (vec-shuffle
                         [[0 2]
                          [1 1]
                          [2 3]
                          [3 0]]
                         [:a :b :c :d]))
    (throws? (vec-shuffle
               [[0 2]
                [1 1]
                [3 0]]
               [:a :b :c :d]))


    (let [ibit-tx-orig [[0 1]
                        [1 2]
                        [2 3]
                        [3 0]]
          data         [:a :b :c :d]]
      (is= data
        (it-> data
          (vec-shuffle ibit-tx-orig it)
          (vec-unshuffle ibit-tx-orig it))))

    (let [ibit-tx-orig [[0 3]
                        [1 2]
                        [2 1]
                        [3 0]]
          data         [:a :b :c :d]]
      (is= data
        (it-> data
          (vec-shuffle ibit-tx-orig it)
          (vec-unshuffle ibit-tx-orig it))))

    (throws? (vec-unshuffle [[0 2]
                             [1 1]
                             [3 0]]
               [:a :b :c :d])))

  (verify
    (doseq [num-bits (thru 4 10)]
      (let [ctx             (new-ctx {:num-bits num-bits})
            N-max           (grab :N-max ctx)
            orig-vals       (range N-max)
            shuffled-vals   (mapv #(shuffle-int-bits ctx %) orig-vals)
            unshuffled-vals (mapv #(unshuffle-int-bits ctx %) shuffled-vals)]
        (is-set= orig-vals shuffled-vals)
        (is= orig-vals unshuffled-vals))))

  #_(verify-focus
      (with-redefs [shuffle-int-bits   (fn [x y] y)
                    unshuffle-int-bits (fn [x y] y)]
        (doseq [ival ;  [3]
                (range 16)
                ]
          (let-spy-pretty
            [; ival 7

             ;  ctx
             ctx    (if true
                      (it-> (new-ctx {:num-bits 4})
                        (glue it {:offset (biginteger 0)
                                  :slope  (biginteger 7)
                                  :L      3
                                  :short  2
                                  }))
                      {:N-max          (biginteger 16)
                       :N-third        5
                       :ibit-tx-orig   [[0 0] [1 3] [2 1] [3 2]]
                       :num-bits       4
                       :num-dec-digits 2
                       :num-hex-digits 1
                       :num-rounds     2
                       :offset         (biginteger 5)
                       :slope          (biginteger 5)})

             cuid   (spyx-pretty (encrypt-frame ctx ival))
             result (spyx-pretty (decrypt-frame ctx cuid))
             ]
            (is= result ival)
            ))))

  ;-----------------------------------------------------------------------------
  (verify
    (let [ctx (new-ctx {:num-bits 32
                        :verbose  visual-debugging?})]
      (with-map-vals ctx [num-bits N-max num-dec-digits num-hex-digits]
        ; arg must be in slice 0..(dec N-max)
        (throws-not? (encrypt-frame ctx 0))
        (throws-not? (encrypt-frame ctx (dec N-max)))
        (throws? (encrypt-frame ctx -1))
        (throws? (encrypt-frame ctx N-max))

        (when visual-debugging?
          (let [cridx-vals (forv [i (take 32 (range N-max))]
                             (int->cuid ctx i))]
            (nl)
            (println "    idx   CUID         hex          binary  ")
            (doseq [[i val] (indexed cridx-vals)]
              (when (neg? val)
                (throw (ex-info "found-negative" (vals->map val))))
              (let [fmt-str (str "%7d  %0" num-dec-digits "d   %s   %s")]
                (println (format fmt-str i val (math/BigInteger->hex-str val num-hex-digits)
                           (int->bitstr val num-bits)))))))))

    ; Fast coverage tests
    (doseq [nbits (thru 4 12)]
      (let [ctx (new-ctx {:num-bits nbits})]
        (with-map-vals ctx [N-max]
          (let [nums-orig     (range N-max)
                nums-shuffled (cp/pmap :builtin #(int->cuid ctx %) nums-orig)]
            (is-set= nums-orig nums-shuffled)))))

    ; Slow coverage test (~35 sec)
    (when false
      (let [ctx (new-ctx {:num-bits 20})]
        (with-map-vals ctx [num-bits N-max]
          (nl)
          (println (format "Running integer coverage test (num-bits: %d  N-max: %d)" num-bits N-max))
          (prof/with-timer-print :coverage-test
            (let [nums-orig     (range N-max)
                  nums-shuffled (cp/pmap :builtin #(int->cuid ctx %) nums-orig)]
              (is-set= nums-orig nums-shuffled)))))))

  (verify
    (when visual-debugging? ; timing printouts disabled by default

      (nl)
      (tsk/with-validation-disabled
        (let [ctx (new-ctx {:num-bits 32})]
          (prof/with-timer-print :timing-1000-32 ; timing for 1000 CRIDX values
            (dotimes [i 1000]
              (int->cuid ctx i))))
        (let [ctx (new-ctx {:num-bits 64})]
          (prof/with-timer-print :timing-1000-64 ; timing for 1000 CRIDX values
            (dotimes [i 1000]
              (int->cuid ctx i))))
        (let [ctx (new-ctx {:num-bits 128})]
          (prof/with-timer-print :timing-1000-128 ; timing for 1000 CRIDX values
            (dotimes [i 1000]
              (int->cuid ctx i))))

        (let [ctx (new-ctx {:num-bits 256})]
          (prof/with-timer-print :timing-1000-256 ; timing for 1000 CRIDX values
            (dotimes [i 1000]
              (int->cuid ctx i))))
        (let [ctx (new-ctx {:num-bits 512})]
          (prof/with-timer-print :timing-1000-512 ; timing for 1000 CRIDX values
            (dotimes [i 1000]
              (int->cuid ctx i))))
        (let [ctx (new-ctx {:num-bits 1024})]
          (tsk/with-validation-disabled)
          (prof/with-timer-print :timing-1000-1024 ; timing for 1000 CRIDX values
            (dotimes [i 1000]
              (int->cuid ctx i)))))))

  )
