(ns tst.cuid.crypt
  (:use tupelo.core
        tupelo.test)
  (:require
    [com.climate.claypoole :as cp]
    [criterium.core :as crit]
    [cuid.crypt :as crypt]
    [schema.core :as s]
    [tupelo.math :as math]
    [tupelo.profile :as prof]
    [tupelo.schema :as tsk]
    ))

(set! *warn-on-reflection* true)

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
    (throws? (crypt/int->bitstr 5 2))
    (is= "101" (crypt/int->bitstr 5 3))
    (is= "0101" (crypt/int->bitstr 5 4))
    (is= "00000101" (crypt/int->bitstr 5 8))))

;-----------------------------------------------------------------------------
(verify
  (is= [:b :c :d :a] (crypt/vec-shuffle
                       [ 1 2 3 0]
                       [:a :b :c :d]))
  (is= [:d :c :b :a] (crypt/vec-shuffle
                       [ 3 2 1 0]
                       [:a :b :c :d]))
  (is= [:c :b :d :a] (crypt/vec-shuffle
                       [ 2 1 3 0]
                       [:a :b :c :d]))
  (throws? (crypt/vec-shuffle
             [2 1 0]
             [:a :b :c :d])))

;-----------------------------------------------------------------------------
(verify
  (doseq [num-bits (thru 4 10)]
    (let [ctx (crypt/new-ctx {:num-bits num-bits})]
      (with-map-vals ctx [num-bits N-max bit-shuffle-idxs-plain bit-shuffle-idxs-crypt]
        (let [orig-vals       (range N-max)
              shuffled-vals   (mapv #(crypt/shuffle-bits-BigInteger num-bits bit-shuffle-idxs-plain %) orig-vals)
              unshuffled-vals (mapv #(crypt/shuffle-bits-BigInteger num-bits bit-shuffle-idxs-crypt %) shuffled-vals)]
          (is-set= orig-vals shuffled-vals)
          (is= orig-vals unshuffled-vals))))))

;-----------------------------------------------------------------------------
(verify

  ; ***** ENABLE TO SEE PRINTOUT *****
  (when false
    (let [ctx (crypt/new-ctx {:num-bits   32
                              :num-rounds 5})]
      ; (spyx ctx)
      (with-map-vals ctx [num-bits N-max num-digits-dec num-digits-hex]
        ; arg must be in slice 0..(dec N-max)
        (throws-not? (crypt/encrypt-frame ctx 1 0))
        (throws-not? (crypt/encrypt-frame ctx 1 (dec N-max)))
        (throws? (crypt/encrypt-frame ctx 1 -1))
        (throws? (crypt/encrypt-frame ctx 1 N-max))

        (let [idx-vals    (take 32 (range N-max))
              cuid-vals   (mapv #(crypt/encrypt ctx %) idx-vals)
              idx-decrypt (mapv #(crypt/decrypt ctx %) cuid-vals)]
          (nl)
          (println "    idx      CUID        hex                 binary                    orig")
          (doseq [[i cuid] (indexed cuid-vals)]
            (when (neg? cuid)
              (throw (ex-info "found-negative" (vals->map cuid))))
            (let [fmt-str (str "%7d   %0" num-digits-dec "d   %s   %s  %7d")
                  hex-str (math/BigInteger->hex-str cuid num-digits-hex)
                  bit-str (crypt/int->bitstr cuid num-bits)]
              (println (format fmt-str i cuid hex-str bit-str (nth idx-decrypt i)))))
          (isnt= idx-vals cuid-vals) ; but not same order (random chance 1 in N!)
          (is= idx-vals idx-decrypt)))))

  ; Fast coverage tests
  (doseq [nbits (thru 4  12)]
    (let [ctx (crypt/new-ctx {:num-bits nbits})]
      (with-map-vals ctx [N-max]
        (let [idx-vals    (range N-max)
              cuid-vals   (cp/pmap :builtin #(crypt/encrypt ctx %) idx-vals)
              idx-decrypt (cp/pmap :builtin #(crypt/decrypt ctx %) cuid-vals)]
          (is-set= idx-vals cuid-vals) ; all vals present
          (isnt= idx-vals cuid-vals) ; but not same order (random chance 1 in N!)
          (is= idx-vals idx-decrypt) ; decryption recovers original vals, in order
          ))))

  ; Slow coverage test (~35 sec)
  (when false
    (let [ctx (crypt/new-ctx {:num-bits 20})]
      (with-map-vals ctx [num-bits N-max]
        (nl)
        (println (format "Running integer coverage test (num-bits: %d  N-max: %d)" num-bits N-max))
        (prof/with-timer-print :coverage-test
          (let [nums-orig     (range N-max)
                nums-shuffled (cp/pmap :builtin #(crypt/encrypt ctx %) nums-orig)]
            (is-set= nums-orig nums-shuffled)))))))

(verify
  (let [times-2 #(* 2 %)]
    (is= [] (take 0 (range 9)))
    (is= 1 (crypt/iterate-n 0 times-2 1))
    (is= 2 (crypt/iterate-n 1 times-2 1))
    (is= 4 (crypt/iterate-n 2 times-2 1))
    (is= 8 (crypt/iterate-n 3 times-2 1))
    (is= 256 (crypt/iterate-n 8 times-2 1))))

(verify
  ; need to uncomment & reformat profile statements in source code to use this

  (when false ; round-trip timing printouts disabled by default

    (tsk/with-validation-disabled

      (prof/timer-stats-reset)
      (let [ctx (crypt/new-ctx {:num-bits 32})]
        (prn :timing-1000-32)
        (dotimes [i 1000] ; timing for 1000 CRIDX values
          (crypt/decrypt ctx
            (crypt/encrypt ctx i))))
      (prof/print-profile-stats)

      (prof/timer-stats-reset)
      (let [ctx (crypt/new-ctx {:num-bits 64})]
        (prn :timing-1000-64)
        (dotimes [i 1000] ; timing for 1000 CRIDX values
          (crypt/decrypt ctx
            (crypt/encrypt ctx i))))
      (prof/print-profile-stats)

      (prof/timer-stats-reset)
      (let [ctx (crypt/new-ctx {:num-bits 128})]
        (prn :timing-1000-128)
        (dotimes [i 1000] ; timing for 1000 CRIDX values
          (crypt/decrypt ctx
            (crypt/encrypt ctx i))))
      (prof/print-profile-stats)

      (prof/timer-stats-reset)
      (let [ctx (crypt/new-ctx {:num-bits 256})]
        (prn :timing-1000-256)
        (dotimes [i 1000] ; timing for 1000 CRIDX values
          (crypt/decrypt ctx
            (crypt/encrypt ctx i))))
      (prof/print-profile-stats)

      )))
