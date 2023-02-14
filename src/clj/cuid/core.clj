(ns cuid.core
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.math :as math]
    [tupelo.schema :as tsk])
  (:import
    [java.util Random]))

;-----------------------------------------------------------------------------
; new:  CUID - Cipher Unique ID
;
; BitScrambler
;  - idx->int
;  - idx->hex
;  - next-int
;  - next-hex

; #todo add fns:  next-biginteger, next-str-dec, next-str-hex

;-----------------------------------------------------------------------------
(def ^:no-doc min-bits 4) ; NOTE! IMPORTANT! 4 bits minimum due to shuffle step
(def ^:no-doc max-bits 1024) ; No real upper limit.  Just process in blocks if desired.

;-----------------------------------------------------------------------------
(s/defn BigInteger->bitchars :- tsk/Vec ; #todo => tupelo.math
  [bi-val :- BigInteger
   bits-width :- s/Int]
  (let [bits-orig         (math/BigInteger->binary-chars bi-val) ; does not include leading zeros
        num-bits-returned (count bits-orig)
        num-leading-zeros (- bits-width num-bits-returned)
        >>                (assert (int-nonneg? num-leading-zeros))
        result            (glue (repeat num-leading-zeros \0) bits-orig)]
    result))

(s/defn BigInteger->bitstr :- s/Str ; #todo => tupelo.math
  [bi-val :- BigInteger
   bits-width :- s/Int]
  (strcat (BigInteger->bitchars bi-val bits-width)))

(s/defn int->bitstr :- s/Str ; #todo => tupelo.math
  [ival :- s/Int
   bits-width :- s/Int]
  (strcat (BigInteger->bitchars (biginteger ival) bits-width)))

;-----------------------------------------------------------------------------
(s/defn ^:no-doc new-ctx-impl :- tsk/KeyMap
  [params :- tsk/KeyMap]
  (with-map-vals params [num-bits rand-seed num-rounds verbose]
    (assert (pos-int? num-bits))
    (assert (pos-int? num-rounds))
    (assert (pos-int? rand-seed))
    (let [random-gen     (Random. rand-seed)

          ; used for text formatting only
          num-hex-digits (long (Math/ceil (/ num-bits 4))) ; (log2 16) => 4
          num-dec-digits (long (Math/ceil (/ num-bits (math/log2 10))))

          ; We want slope & offset to be in the central third of all possible values to
          ; "encourage" lots of bits to flip on each multiply/add operation.
          N-max          (math/pow-BigInteger 2 num-bits)
          N-third        (.divide N-max (biginteger 3))

          offset         (biginteger
                           (it-> (.nextDouble random-gen)
                             (* it N-third)
                             (+ it N-third)))

          ; ***** MUST BE ODD *****  Thus, it is relatively prime to N-max (power of 2 => even)
          slope          (biginteger
                           (it-> (.nextDouble random-gen)
                             (* it N-third)
                             (+ it N-third)
                             (biginteger it)
                             (if (even? it) ; ensure it is odd
                                 (.add it (biginteger 1))
                                 it)))

          ; #todo extract to a function & write unit tests
          bit-order      (let [K            (Math/round (Math/sqrt num-bits))
                               bit-seqs     (partition-all K (range num-bits))
                               bit-seqs-rev (forv [[i bit-seq] (indexed bit-seqs)]
                                              (if (odd? i)
                                                (reverse bit-seq)
                                                bit-seq))
                               result       (vec (apply interleave-all bit-seqs-rev))] ; example [0 7 8 15 1 6 9 14 2 5 10 13 3 4 11 12]
                           (assert (= (set (range num-bits)) (set result)))
                           result)]

      (when-not (<= min-bits num-bits max-bits)
        (throw (ex-info "num-bits out of range " (vals->map num-bits min-bits max-bits))))

      ;-----------------------------------------------------------------------------
      ; sanity checks
      (assert (pos-int? num-rounds))
      (assert (biginteger? offset))
      (assert (biginteger? slope))
      (when-not (odd? slope) ; odd => relatively prime to 2^N
        (throw (ex-info "slope failed even test" (vals->map slope))))

      (when verbose
        (spyx num-bits)
        (spyx num-hex-digits)
        (spyx N-max)
        (spyx [offset (math/BigInteger->binary-str offset)])
        (spyx [slope (math/BigInteger->binary-str slope)])
        (spyx bit-order))

      (vals->map num-bits num-rounds num-dec-digits num-hex-digits N-max N-third
        offset slope bit-order
        ))))

(s/defn new-ctx :- tsk/KeyMap
  "Creates a new CUID context map. Usage:

        (new-ctx <params-map>)

  where <params-map> is of the form:

        {:num-bits     <n>      ; REQUIRED (minimum: 4)
         :rand-seed    <long>   ; the CUID encryption key (default: randomized)
         :num-rounds   <int>    ; must be a positive int  (default: 2)
         :verbose      false    ; enable for dbg prints
        }
  "
  [arg :- tsk/KeyMap]
  (s/validate {:num-bits                    s/Int
               (s/optional-key :rand-seed)  s/Int
               (s/optional-key :num-rounds) s/Int
               (s/optional-key :verbose)    s/Bool}
    arg)
  (let [params-default {:rand-seed  (Math/abs (.nextLong (Random.))) ; positive for simplicity
                        :num-rounds 2
                        :verbose    false}
        params         (glue params-default (if (int? arg)
                                              {:num-bits arg}
                                              arg))
        ctx            (new-ctx-impl params)]
    ctx))

;-----------------------------------------------------------------------------
(s/defn ^:no-doc shuffle-bits :- BigInteger ; #todo need unshuffle-bits
  [ctx :- tsk/KeyMap
   ival :- BigInteger]
  (with-map-vals ctx [N-max num-bits bit-order]
    (when-not (and (<= 0 ival) (< ival N-max))
      (throw (ex-info "ival out of range" (vals->map ival N-max))))
    (let [bits-full (BigInteger->bitchars ival num-bits)
          bits-out  (forv [i (range num-bits)]
                      (let [isrc    (get bit-order i)
                            bit-val (get bits-full isrc)]
                        bit-val))
          result    (math/binary-chars->BigInteger bits-out)]
      result)))

(s/defn ^:no-doc encrypt-frame :- BigInteger ; #todo need decrypt-frame
  [ctx :- tsk/KeyMap
   ival :- s/Int]
  (with-map-vals ctx [N-max slope offset]
    (when-not (and (<= 0 ival) (< ival N-max))
      (throw (ex-info "ival out of range" (vals->map ival N-max))))
    (let [x1 (biginteger ival)
          x2 (.multiply ^BigInteger x1 slope)
          x3 (.add ^BigInteger offset x2)
          x4 (.mod ^BigInteger x3 N-max)
          x5 (shuffle-bits ctx x4)]
      x5)))

; Timing (2 rounds):
;   32 bits:  15 usec/call
;   64 bits:  25 usec/call
;  128 bits:  42 usec/call
;  256 bits:  80 usec/call
;  512 bits: 150 usec/call
; 1024 bits: 300 usec/call
(s/defn int->cuid :- BigInteger
  [ctx :- tsk/KeyMap
   ival :- s/Int]
  (with-map-vals ctx [num-rounds]
    (biginteger
      (nth
        (iterate #(encrypt-frame ctx %) ival) ; NOTE: seq is [x  (f x)  (f (f x))...] so don't use (dec N)
        num-rounds)))) ;

