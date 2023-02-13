(ns cridx.core
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [tupelo.math :as math]
    [tupelo.schema :as tsk])
  (:import
    [java.util Random]))

;---------------------------------------------------------------------------------------------------
; new:  CRINT - Cipher Random Integer
;
; BitScrambler
;  - idx->int
;  - idx->hex
;  - next-int
;  - next-hex

; #todo add fns:  next-biginteger, next-str-dec, next-str-hex

(def min-bits 4) ; NOTE! important! 4 bits minimum
(def max-bits 1024) ; no real upper limit

(def num-bits 32)
(def num-rounds 2) ; must be non-zero!

(def num-dec-digits (long (Math/ceil (/ num-bits (math/log2 10)))))
(def num-hex-digits (long (Math/ceil (/ num-bits 4)))) ; (log2 16) => 4
(def N-max (math/pow-BigInteger 2 num-bits))
(def N-third (.divide N-max (biginteger 3)))
(spyx num-hex-digits)
(spyx num-bits)
(spyx N-max)

(s/defn int->bitchars :- tsk/Vec ; #todo => tupelo.math
  [bi-orig :- s/Int
   bits-width :- s/Int]
  (let [bits-orig         (math/BigInteger->binary-chars (biginteger bi-orig)) ; does not include leading zeros
        num-bits-returned (count bits-orig)
        num-leading-zeros (- bits-width num-bits-returned)
        >>                (assert (int-nonneg? num-leading-zeros))
        result            (glue (repeat num-leading-zeros \0) bits-orig)]
    result))

(s/defn int->bitstr :- s/Str ; #todo => tupelo.math
  [bi-orig :- s/Int
   bits-width :- s/Int]
  (strcat (int->bitchars bi-orig bits-width)))

(when-not (<= min-bits num-bits max-bits)
  (throw (ex-info "num-bits out of range " (vals->map num-bits min-bits max-bits))))

;-----------------------------------------------------------------------------
; initialization

(def seed 777717777)
(def random
  (if false
    (Random. seed)
    (Random.)))

(def offset (it-> (.nextDouble random)
              (* it N-third)
              (+ it N-third)
              (biginteger it)))

(def increment ; ***** MUST BE ODD *****
  (biginteger
    (it-> (.nextDouble random)
      (* it N-third)
      (+ it N-third)
      (biginteger it)
      (if (even? it) ; ensure it is odd
          (.add ^BigInteger it (biginteger 1))
          it))))

;-----------------------------------------------------------------------------
; sanity checks
(assert (pos-int? num-rounds))
(assert (biginteger? offset))
(assert (biginteger? increment))
(when-not (odd? increment) ; odd => relatively prime to 2^N
  (throw (ex-info "increment failed even test" (vals->map increment))))

(spyx [offset (math/BigInteger->binary-str offset)])
(spyx [increment (math/BigInteger->binary-str increment)])

;-----------------------------------------------------------------------------
(def ^:no-doc bit-order
  (let [K            (Math/round (Math/sqrt num-bits))
        bit-seqs     (partition-all K (range num-bits))
        bit-seqs-rev (forv [[i bit-seq] (indexed bit-seqs)]
                       (if (odd? i)
                         (reverse bit-seq)
                         bit-seq))
        result       (vec (apply interleave-all bit-seqs-rev))] ; example [0 7 8 15 1 6 9 14 2 5 10 13 3 4 11 12]
    (assert (= (set (range num-bits)) (set result)))
    result))
(spyx bit-order)

(s/defn ^:no-doc shuffle-bits :- BigInteger ; #todo need unshuffle-bits
  [arg :- BigInteger]
  (when-not (and (<= 0 arg) (< arg N-max))
    (throw (ex-info "arg out of range" (vals->map arg N-max))))
  (let [bits-full (int->bitchars arg num-bits)
        bits-out  (forv [i (range num-bits)]
                    (let [isrc    (get bit-order i)
                          bit-val (get bits-full isrc)]
                      bit-val))
        result    (math/binary-chars->BigInteger bits-out)]
    result))

(s/defn ^:no-doc encrypt-frame :- BigInteger
  [idx :- s/Int]
  (when-not (and (<= 0 idx) (< idx N-max))
    (throw (ex-info "arg out of range" (vals->map idx N-max))))
  (let [x1 (biginteger idx)
        x2 (.multiply ^BigInteger x1 increment)
        x3 (.add ^BigInteger offset x2)
        x4 (.mod ^BigInteger x3 N-max)
        x5 (shuffle-bits x4)]
    x5))

; Timing (2 rounds):
;   32 bits:  20 usec/call
;   64 bits:  40 usec/call
;  128 bits:  55 usec/call
;  256 bits: 110 usec/call
;  512 bits: 260 usec/call
; 1024 bits: 600 usec/call
(s/defn int->crint :- BigInteger
  [idx :- s/Int]
  (biginteger
    (nth
      (iterate encrypt-frame idx) ; NOTE: seq is [x  (f x)  (f (f x))...] so don't use (dec N)
      num-rounds))) ;


