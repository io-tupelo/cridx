(ns tst.cuid.core
  (:use cuid.core
        tupelo.core
        tupelo.test)
  (:require
    [com.climate.claypoole :as cp]
    [tupelo.java-time :as jt]
    [tupelo.math :as math]
    [tupelo.profile :as prof]
    [tupelo.string :as str]
    ))

(set! *warn-on-reflection* true)

(verify
  (throws? (int->bitstr 5 2))
  (is= "101" (int->bitstr 5 3))
  (is= "0101" (int->bitstr 5 4))
  (is= "00000101" (int->bitstr 5 8)))

(verify
  (let [ctx (new-ctx 32)]
    (with-map-vals ctx [num-bits N-max num-dec-digits num-hex-digits]

      (when true ; visual dubugging
        (nl)
        (let [cridx-vals (prof/with-timer-print :table-print ; timing for 1000 CRIDX values
                           (forv [i (take 1000 (range N-max))]
                             (int->cuid ctx i)))]
          (when false ; verbose
            (nl)
            (println "    idx   cridx    hex     binary  ")
            (doseq [[i val] (indexed cridx-vals)]
              (when (neg? val)
                (throw (ex-info "found-negative" (vals->map val))))
              (let [fmt-str (str "%7d  %0" num-dec-digits "d   %s   %s")]
                (println (format fmt-str i val (math/BigInteger->hex-str val num-hex-digits)
                           (int->bitstr val num-bits))))))))

      ; arg must be in slice [0..N-max)
      (throws-not? (encrypt-frame ctx 0))
      (throws-not? (encrypt-frame ctx (dec N-max)))
      (throws? (encrypt-frame ctx -1))
      (throws? (encrypt-frame ctx N-max))

      (newline)
      (if (< N-max (math/pow-long 2 21))
        (do ; then
          (println "Running integer coverage test (parallel)...")
          (newline)
          (prof/with-timer-print :coverage-test
            (let [nums-orig     (range N-max)
                  nums-shuffled (cp/pmap :builtin int->cuid nums-orig)]
              (is-set= nums-orig nums-shuffled))))
        (do ; else
          (print "Skipping integer coverage test.")
          (newline))))))

