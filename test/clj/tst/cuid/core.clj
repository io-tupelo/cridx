(ns tst.cuid.core
  (:use cuid.core tupelo.core tupelo.test)
  (:require
    [com.climate.claypoole :as cp]
    [tupelo.math :as math]
    [tupelo.profile :as prof]
    [tupelo.schema :as tsk]
    [tupelo.string :as str]
    ))

(set! *warn-on-reflection* true)

(def visual-debugging? false ) ; <= enable to see extra printouts

(verify
  (throws? (int->bitstr 5 2))
  (is= "101" (int->bitstr 5 3))
  (is= "0101" (int->bitstr 5 4))
  (is= "00000101" (int->bitstr 5 8)))

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
  (doseq [nbits [8 9 10 11 12]]
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
            (is-set= nums-orig nums-shuffled))))))
  )

(verify
  (when false ; timing printouts disabled by default

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
