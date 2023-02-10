(ns tst.demo.core
  (:use demo.core tupelo.core tupelo.test)
  (:require
    [tupelo.math :as math]
    [tupelo.profile :as prof]
    )
  (:import
    [java.math BigInteger]))

(set! *warn-on-reflection* true)

(dotest
  (throws? (BigInteger->bitstr 5 2))
  (is= "101" (BigInteger->bitstr 5 3))
  (is= "0101" (BigInteger->bitstr 5 4))
  (is= "00000101" (BigInteger->bitstr 5 8))
  )

(dotest
  (when true ; visual dubugging
    (nl)
    (println "    idx   cridx    hex     binary  ")
    (forv [i (take 44 (range N-max))]
      (let [val (idx-shuffle i)]
        (when (neg? val)
          (throw (ex-info "found-negative" (vals->map val))))
        (when verbose?
          (let [fmt-str (str "%7d  %0" num-dec-digits "d   %s   %s")]
            (println (format fmt-str i val (math/BigInteger->hex-str val num-hex-digits)
                       (BigInteger->bitstr val num-bits)
                       )))))))

  ; arg must be in slice [0..N-max)
  (throws-not? (idx-shuffle-round 0))
  (throws-not? (idx-shuffle-round (dec N-max)))
  (throws? (idx-shuffle-round -1))
  (throws? (idx-shuffle-round N-max))

  (newline)
  (if (< N-max (math/pow-long 2 21))
    (do
      (println "Running integer coverage test...")
      (prof/with-timer-print :coverage
        (let [nums-orig     (range N-max)
              nums-shuffled (vec (pmap idx-shuffle nums-orig))]
          (is-set= nums-orig nums-shuffled))))
    ; else
    (print "Skipping integer coverage test."))
  (newline)
  )
