(ns tst.cuid.core
  (:use cuid.core
        tupelo.core
        tupelo.test)
  (:require
    [com.climate.claypoole :as cp]
    [tupelo.java-time :as jt]
    [tupelo.math :as math]
    [tupelo.profile :as prof]
    [tupelo.schema :as tsk]
    [tupelo.string :as str]
    ))

(set! *warn-on-reflection* true)

(verify
  (throws? (int->bitstr 5 2))
  (is= "101" (int->bitstr 5 3))
  (is= "0101" (int->bitstr 5 4))
  (is= "00000101" (int->bitstr 5 8)))

(verify
  (nl)
  (let [ctx (new-ctx {:num-bits 32
                      :verbose  true})]
    (with-map-vals ctx [num-bits N-max num-dec-digits num-hex-digits]
      ; arg must be in slice 0..(dec N-max)
      (throws-not? (encrypt-frame ctx 0))
      (throws-not? (encrypt-frame ctx (dec N-max)))
      (throws? (encrypt-frame ctx -1))
      (throws? (encrypt-frame ctx N-max))

      ; visual dubugging
      (when false
        (let [cridx-vals (forv [i (take 50 (range N-max))]
                           (int->cuid ctx i))]
          (nl)
          (println "    idx   CUID         hex          binary  ")
          (doseq [[i val] (indexed cridx-vals)]
            (when (neg? val)
              (throw (ex-info "found-negative" (vals->map val))))
            (let [fmt-str (str "%7d  %0" num-dec-digits "d   %s   %s")]
              (println (format fmt-str i val (math/BigInteger->hex-str val num-hex-digits)
                         (int->bitstr val num-bits)))))))))

  (let [ctx (new-ctx {:num-bits 16})]
    (with-map-vals ctx [num-bits N-max]
      (newline)
      (println (format "Running integer coverage test (num-bits: %d  N-max: %d)" num-bits N-max))
      (prof/with-timer-print :coverage-test
        (let [nums-orig     (range N-max)
              nums-shuffled (cp/pmap :builtin #(int->cuid ctx %) nums-orig)]
          (is-set= nums-orig nums-shuffled)))
      )))

(verify
  (let [ctx (new-ctx {:num-bits 32})]
    (tsk/with-validation-disabled
      (with-map-vals ctx [N-max]
        (prof/with-timer-print :timing-1000-32 ; timing for 1000 CRIDX values
          (doseq [i (take 1000 (range N-max))]
            (int->cuid ctx i))))))
  (let [ctx (new-ctx {:num-bits 64})]
    (tsk/with-validation-disabled
      (with-map-vals ctx [N-max]
        (prof/with-timer-print :timing-1000-64 ; timing for 1000 CRIDX values
          (doseq [i (take 1000 (range N-max))]
            (int->cuid ctx i))))))
  (let [ctx (new-ctx {:num-bits 128})]
    (tsk/with-validation-disabled
      (with-map-vals ctx [N-max]
        (prof/with-timer-print :timing-1000-128 ; timing for 1000 CRIDX values
          (doseq [i (take 1000 (range N-max))]
            (int->cuid ctx i))))))

  (when false
    (let [ctx (new-ctx {:num-bits 256})]
      (tsk/with-validation-disabled
        (with-map-vals ctx [N-max]
          (prof/with-timer-print :timing-1000-256 ; timing for 1000 CRIDX values
            (doseq [i (take 1000 (range N-max))]
              (int->cuid ctx i))))))
    (let [ctx (new-ctx {:num-bits 512})]
      (tsk/with-validation-disabled
        (with-map-vals ctx [N-max]
          (prof/with-timer-print :timing-1000-512 ; timing for 1000 CRIDX values
            (doseq [i (take 1000 (range N-max))]
              (int->cuid ctx i))))))
    (let [ctx (new-ctx {:num-bits 1024})]
      (tsk/with-validation-disabled
        (with-map-vals ctx [N-max]
          (prof/with-timer-print :timing-1000-1024 ; timing for 1000 CRIDX values
            (doseq [i (take 1000 (range N-max))]
              (int->cuid ctx i))))))))
