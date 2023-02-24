(ns tst.cuid.crypt
  (:use cuid.crypt
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
(def verbose? false)

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
  (let [N 32]
    (doseq [m (range 1 16 2)]
      (when verbose?
        (nl)
        (prn (vals->map m)))
      (doseq [x (range 16)]
        (let [c      (crypt N m x)
              result (decrypt N m c)]
          (when verbose?
            (prn (vals->map x c result)))
          (is= x result))))))

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

;-----------------------------------------------------------------------------
(verify
  (doseq [num-bits (thru 4 10)]
    (let [ctx             (new-ctx {:num-bits num-bits})
          N-max           (grab :N-max ctx)
          orig-vals       (range N-max)
          shuffled-vals   (mapv #(shuffle-bits-BigInteger ctx %) orig-vals)
          unshuffled-vals (mapv #(unshuffle-bits-BigInteger ctx %) shuffled-vals)]
      (is-set= orig-vals shuffled-vals)
      (is= orig-vals unshuffled-vals))))

;-----------------------------------------------------------------------------
(verify

  ; enable to see printout
  (when false
    (let [ctx (new-ctx {:num-bits   32
                        :num-rounds 5})]
      (with-map-vals ctx [num-bits N-max num-digits-dec num-digits-hex]
        ; arg must be in slice 0..(dec N-max)
        (throws-not? (encrypt-frame ctx 0))
        (throws-not? (encrypt-frame ctx (dec N-max)))
        (throws? (encrypt-frame ctx -1))
        (throws? (encrypt-frame ctx N-max))

        (let [idx-vals    (take 32 (range N-max))
              cuid-vals   (mapv #(int->cuid ctx %) idx-vals)
              idx-decrypt (mapv #(cuid->int ctx %) cuid-vals)]
          (nl)
          (println "    idx   CUID         hex          binary                              orig  ")
          (doseq [[i cuid] (indexed cuid-vals)]
            (when (neg? cuid)
              (throw (ex-info "found-negative" (vals->map cuid))))
            (let [fmt-str (str "%7d  %0" num-digits-dec "d   %s   %s  %7d")
                  hex-str (math/BigInteger->hex-str cuid num-digits-hex)
                  bit-str (int->bitstr cuid num-bits)]
              (println (format fmt-str i cuid hex-str bit-str (nth idx-decrypt i)))))
          (isnt= idx-vals cuid-vals)
          (is= idx-vals idx-decrypt)))))

  ; Fast coverage tests
  (doseq [nbits (thru 4 12)]
    (let [ctx (new-ctx {:num-bits nbits})]
      (with-map-vals ctx [N-max]
        (let [idx-vals    (range N-max)
              cuid-vals   (cp/pmap :builtin #(int->cuid ctx %) idx-vals)
              idx-decrypt (cp/pmap :builtin #(cuid->int ctx %) cuid-vals)]
          (is-set= idx-vals cuid-vals) ; all vals present
          (isnt= idx-vals cuid-vals) ; but not same order (almost surely)
          (is= idx-vals idx-decrypt) ; decryption recovers original vals, in order
          ))))

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

(comment
  #_(verify-focus
      (with-redefs [shuffle-bits-BigInteger   (fn [x y] y)
                    unshuffle-bits-BigInteger (fn [x y] y)]
        (doseq [ival ;  [3]
                (range 16)
                ]
          (let-spy-pretty
            [; ival 7
             ctx    (new-ctx {:num-bits 4})

             cuid   (spyx-pretty (encrypt-frame ctx ival))
             result (spyx-pretty (decrypt-frame ctx cuid))
             ]
            (is= result ival)
            ))))

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
