(ns tst.cridx.core
  (:use cridx.core tupelo.core tupelo.test)
  (:require
    [com.climate.claypoole :as cp]
    [tupelo.math :as math]
    [tupelo.profile :as prof]
    ))

(set! *warn-on-reflection* true)

(verify
  (throws? (BigInteger->bitstr 5 2))
  (is= "101" (BigInteger->bitstr 5 3))
  (is= "0101" (BigInteger->bitstr 5 4))
  (is= "00000101" (BigInteger->bitstr 5 8)))

(verify
  (when true ; visual dubugging
    (nl)
    (let [cridx-vals (prof/with-timer-print :table-print ; timing for 1000 CRIDX values
                       (forv [i (take 1000 (range N-max))]
                         (idx-shuffle i)))]
      (when false ; verbose
        (nl)
        (println "    idx   cridx    hex     binary  ")
        (doseq [[i val] (indexed cridx-vals)]
          (when (neg? val)
            (throw (ex-info "found-negative" (vals->map val))))
          (let [fmt-str (str "%7d  %0" num-dec-digits "d   %s   %s")]
            (println (format fmt-str i val (math/BigInteger->hex-str val num-hex-digits)
                       (BigInteger->bitstr val num-bits))))))))

  ; arg must be in slice [0..N-max)
  (throws-not? (idx-shuffle-round 0))
  (throws-not? (idx-shuffle-round (dec N-max)))
  (throws? (idx-shuffle-round -1))
  (throws? (idx-shuffle-round N-max))

  (newline)
  (if (< N-max (math/pow-long 2 21))
    (do   ; then
      (println "Running integer coverage test (parallel)...")
      (newline)
      (prof/with-timer-print :coverage-test
        (let [nums-orig     (range N-max)
              nums-shuffled (cp/pmap :builtin idx-shuffle nums-orig)]
          (is-set= nums-orig nums-shuffled))))
    (do   ; else
      (print "Skipping integer coverage test.")
      (newline))))

