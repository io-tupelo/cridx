(ns tst.demo.core
  (:use demo.core tupelo.core tupelo.test)
  (:require
    [tupelo.math :as math]
    ))

(set! *warn-on-reflection* true)

(dotest
  (when true ; visual dubugging
    (forv [i (take 44 (range N-max))]
      (let [val (idx-shuffle i)]
        (when (neg? val)
          (throw (ex-info "found-negative" (vals->map val))))
        (when verbose?
          (let [fmt-str (str "%7d  %0" num-dec-digits "d   %s")]
            (println (format fmt-str i val (math/BigInteger->hex-str val num-hex-digits))))))))

  ; arg must be in slice [0..N-max)
  (throws-not? (idx-shuffle-round 0))
  (throws-not? (idx-shuffle-round (dec N-max)))
  (throws? (idx-shuffle-round -1))
  (throws? (idx-shuffle-round N-max))

  (when (< N-max (math/pow-long 2 21))
    (let [nums-orig     (range N-max)
          nums-shuffled (mapv idx-shuffle nums-orig)]
      (is-set= nums-orig nums-shuffled))))
