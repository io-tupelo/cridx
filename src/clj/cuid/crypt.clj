(ns cuid.crypt
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [cuid.modular-arithmetic :as mod]
    [tupelo.math :as math]
    [tupelo.profile :as prof]
    [tupelo.schema :as tsk]
    [tupelo.string :as str])
  (:import
    [java.util Random]
    ))

(def Matrix [[s/Any]])
(def verbose? false)

;---------------------------------------------------------------------------------------------------
; :timing-1000-128 {:num-rounds 5  :shuffle-bits? false}
;
; Profile Stats:
; Samples       TOTAL        MEAN      SIGMA           ID
; 1000        0.011     0.000011   0.000003   :cuid->idx
; 1000        0.010     0.000010   0.000003   :idx->cuid
;
;-----------------------------------------------------------------------------
; :timing-1000-128 {:num-rounds 5  :shuffle-bits? true}
;
; Profile Stats:
; Samples       TOTAL        MEAN      SIGMA           ID
; 1000        0.264     0.000264   0.000055   :cuid->idx
; 1000        0.238     0.000238   0.000061   :idx->cuid
; 5000        0.221     0.000044   0.000024   :shuffle-bits-BigInteger
; 5000        0.043     0.000009   0.000002   :shuffle-bits-BigInteger-a
; 5000        0.129     0.000026   0.000017   :shuffle-bits-BigInteger-b
; 5000        0.029     0.000006   0.000016   :shuffle-bits-BigInteger-c
; 5000        0.246     0.000049   0.000019   :unshuffle-bits-BigInteger
; 5000        0.044     0.000009   0.000003   :unshuffle-bits-BigInteger-a
; 5000        0.155     0.000031   0.000017   :unshuffle-bits-BigInteger-b
; 5000        0.029     0.000006   0.000002   :unshuffle-bits-BigInteger-c
;---------------------------------------------------------------------------------------------------

;-----------------------------------------------------------------------------
(def ^:no-doc min-bits 4) ; NOTE! IMPORTANT! 4 bits minimum due to shuffle step
(def ^:no-doc max-bits 1024) ; No real upper limit.  Just process in blocks if desired.

;-----------------------------------------------------------------------------
(s/defn int->bitchars :- tsk/Vec ; #todo => tupelo.math
  [ival :- s/Int
   bits-width :- s/Int]
  (let [bitchars-orig     (math/BigInteger->binary-chars (biginteger ival)) ; does not include leading zeros
        num-bitchars      (count bitchars-orig)
        num-leading-zeros (- bits-width num-bitchars)
        >>                (assert (int-nonneg? num-leading-zeros))
        bitchars-final    (glue (repeat num-leading-zeros \0) bitchars-orig)]
    bitchars-final))

(s/defn int->bitstr :- s/Str ; #todo => tupelo.math
  [ival :- s/Int
   bits-width :- s/Int]
  (str/join (int->bitchars ival bits-width)))

(s/defn iterate-n :- s/Any
  [N :- s/Int
   f :- tsk/Fn
   x :- s/Any]
  (assert (int-nonneg? N))
  (last
    (take (inc N) ; (take 0 <seq>) returns [], so we need (inc N) here to get a result
      (iterate f x))))

;-----------------------------------------------------------------------------
; #todo: maybe make more general version?
(s/defn ^:no-doc vec-shuffle :- tsk/Vec
  [bit-idxs :- [s/Int]
   vec-orig :- tsk/Vec]
  (assert (= (count bit-idxs) (count vec-orig)))
  (forv [idx bit-idxs]
    (get vec-orig idx ::error-81)))

;-----------------------------------------------------------------------------
(s/defn ^:no-doc shuffle-bits-BigInteger :- BigInteger
  [num-bits :- s/Int
   bit-shuffle-idxs :- [s/Int]
   ival :- s/Int]
  (it-> ival
    (int->bitchars it num-bits)
    (vec-shuffle bit-shuffle-idxs it)
    (math/binary-chars->BigInteger it)))

;-----------------------------------------------------------------------------
(s/defn ^:no-doc encrypt-frame :- BigInteger
  [ctx :- tsk/KeyMap
   iround :- s/Int
   ival :- s/Int]
  (with-map-vals ctx [num-bits N-max slopes offsets shuffle-bits?  bit-shuffle-idxs-plain]
    (when-not (and (<= 0 ival) (< ival N-max))
      (throw (ex-info "ival out of range" (vals->map ival N-max))))
    ; calculate mod( y = mx + b ), then shuffle bits
    (let [slope  (get slopes iround)
          offset (get offsets iround)
          ival   (biginteger ival)
          r1     (it-> ival
                   (.multiply ^BigInteger it slope)
                   (.add ^BigInteger it offset)
                   (mod/mod-BigInteger it N-max))
          r2     (cond-it-> r1
                   shuffle-bits? (shuffle-bits-BigInteger num-bits bit-shuffle-idxs-plain it))]
      r2)))

(s/defn ^:no-doc decrypt-frame :- BigInteger
  [ctx :- tsk/KeyMap
   iround :- s/Int
   cuid :- s/Int]
  (with-map-vals ctx [num-bits N-max slopes-inv offsets shuffle-bits? bit-shuffle-idxs-crypt]
    (when-not (and (<= 0 cuid) (< cuid N-max))
      (throw (ex-info "cuid out of range" (vals->map cuid N-max))))
    (let [slope-inv (get slopes-inv iround)
          offset    (get offsets iround)
          cuid      (biginteger cuid)
          r1        (cond-it-> cuid
                      shuffle-bits? (shuffle-bits-BigInteger num-bits bit-shuffle-idxs-crypt it))
          r2        (it-> r1
                      (.subtract ^BigInteger it ^BigInteger offset)
                      (.multiply ^BigInteger it ^BigInteger slope-inv)
                      (mod/mod-BigInteger it N-max))]
      r2)))

(s/defn gen-slope :- BigInteger
  "Generate a positive, odd slope value"
  [nbits :- s/Int
   random-gen :- Random]
  (assert (pos? nbits))
  (let [bound  (math/pow-BigInteger 2 nbits)
        result (it-> (dec nbits)
                 (BigInteger. it random-gen)
                 (* 2 it)
                 (inc it)
                 (biginteger it))]
    (assert (pos? result))
    (assert (odd? result))
    (assert (< result bound))
    result))

;-----------------------------------------------------------------------------
(s/defn ^:no-doc new-ctx-impl :- tsk/KeyMap
  [params :- tsk/KeyMap]
  (with-map-vals params [num-bits rand-seed num-rounds]
    (assert (pos-int? num-bits))
    (assert (pos-int? num-rounds))
    (assert (pos-int? rand-seed))
    (let [random-gen             (Random. rand-seed)

          ; used for text formatting only
          num-digits-hex         (long (Math/ceil (/ num-bits 4))) ; (log2 16) => 4
          num-digits-dec         (long (Math/ceil (/ num-bits (math/log2 10))))

          ; We want slope & offset to be in the central half of all possible values to
          ; "encourage" lots of bits to flip on each multiply/add operation.
          N-max                  (math/pow-BigInteger 2 num-bits)

          offsets                (forv [i (range num-rounds)]
                                   (BigInteger. num-bits random-gen))
          slopes                 (forv [i (range num-rounds)]
                                   (gen-slope num-bits random-gen))
          slopes-inv             (forv [slope slopes]
                                   (biginteger (mod/mod-inverse slope N-max)))

          round-idxs             (vec (range num-rounds)) ; precompute since used on every call
          round-idxs-rev         (vec (reverse round-idxs))

          ;-----------------------------------------------------------------------------
          ; #todo extract to a function & write unit tests
          K                      (Math/round (Math/sqrt num-bits))
          bit-idxs-2d            (partition-all K (range num-bits))
          bit-idxs-2d-rev        (forv [[irow ibit-seq] (indexed bit-idxs-2d)]
                                   (if (odd? irow)
                                     (reverse ibit-seq)
                                     ibit-seq))
          bit-shuffle-idxs-plain (vec (apply interleave-all bit-idxs-2d-rev)) ; example [0 7 8 15 1 6 9 14 2 5 10 13 3 4 11 12]
          bit-shuffle-idxs-crypt (it-> (indexed bit-shuffle-idxs-plain)
                                   (sort-by second it)
                                   (mapv first it))
          ;-----------------------------------------------------------------------------
          ]
      (assert (= (set (range num-bits)) (set bit-shuffle-idxs-plain)))
      (assert (= (set (range num-bits)) (set bit-shuffle-idxs-crypt)))

      ;-----------------------------------------------------------------------------
      ; sanity checks
      (when-not (<= min-bits num-bits max-bits)
        (throw (ex-info "num-bits out of range " (vals->map num-bits min-bits max-bits))))
      ; (assert (biginteger? offset))
      ; (assert (biginteger? slope))
      #_(when-not (odd? slope) ; odd => relatively prime to 2^N
          (throw (ex-info "slope failed even test" (vals->map slope))))

      (when verbose?
        (spyx num-bits)
        (spyx num-digits-hex)
        (spyx N-max)
        (spyx offsets)
        (spyx slopes)
        (spyx slopes-inv)
        (spyx bit-shuffle-idxs-plain)
        (spyx bit-shuffle-idxs-crypt)
        )

      (let [ctx-out (glue params
                      (vals->map num-bits num-rounds num-digits-dec num-digits-hex N-max
                        offsets slopes slopes-inv round-idxs round-idxs-rev
                        bit-shuffle-idxs-plain bit-shuffle-idxs-crypt
                        ))]
        ctx-out))))

(s/defn new-ctx :- tsk/KeyMap
  "Creates a new encryption context map. Usage:

        (new-ctx <params-map>)

   where <params-map> is of the form:

        {:num-bits     <long>  ; REQUIRED:  (minimum: 4): input/output integers in [0..2^n)
         :rand-seed    <long>  ; optional:  encryption key (default: randomized)
         :num-rounds   <long>  ; optional:  positive int (default: 3)
        } "
  [opts :- tsk/KeyMap]
  (s/validate {:num-bits                       s/Int
               (s/optional-key :rand-seed)     s/Int
               (s/optional-key :num-rounds)    s/Int
               (s/optional-key :shuffle-bits?) s/Bool
               s/Any s/Any }
    opts)
  (let [opts-default {:rand-seed     (Math/abs (.nextLong (Random.))) ; positive for simplicity
                      :num-rounds    3
                      :shuffle-bits? true}
        ctx          (new-ctx-impl (glue opts-default opts))]
    ctx))

;-----------------------------------------------------------------------------
; Timing {:num-rounds 7  :shuffle-bits? false}
;   32 bits:  12 usec/call
;   64 bits:  12 usec/call
;  128 bits:  12 usec/call
;
; Timing {:num-rounds 3  :shuffle-bits? true}
;   32 bits:  23 usec/call
;   64 bits:  35 usec/call
;  128 bits:  60 usec/call
;  256 bits: 120 usec/call
(s/defn encrypt :- BigInteger
  "Given an encryption context map, encrypts an N-bit integer, returning
   the N-bit encrypted value."
  [ctx :- tsk/KeyMap
   int-plain :- s/Int]
  ; (prof/with-timer-accum :idx->cuid)
  (reduce
    (fn [result round]
      (encrypt-frame ctx round result))
    (biginteger int-plain)
    (grab :round-idxs ctx)))

(s/defn decrypt :- BigInteger
  "Given an encryption context map, decrypts an N-bit integer, returning
   the N-bit original value."
  [ctx :- tsk/KeyMap
   int-crypt :- s/Int]
  ; (prof/with-timer-accum :int-crypt->idx)
  (reduce
    (fn [result round]
      (decrypt-frame ctx round result))
    (biginteger int-crypt)
    (grab :round-idxs-rev ctx)))

