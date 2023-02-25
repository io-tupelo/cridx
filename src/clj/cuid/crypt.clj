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
    [java.util Random]))

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

(def ^:no-doc bi-0 (biginteger 0))
(def ^:no-doc bi-1 (biginteger 1))

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
  [bit-shuffle-idxs :- Matrix
   vec-orig :- tsk/Vec]
  (assert (= (count bit-shuffle-idxs) (count vec-orig)))
  ; Since `bit-shuffle-idxs` is sorted by icrypt, we can just go in sequence
  (let [vec-shuffled (forv [[icrypt iplain] bit-shuffle-idxs]
                       (get vec-orig iplain ::error))]
    vec-shuffled))


(s/defn ^:no-doc vec-unshuffle :- tsk/Vec
  [bit-shuffle-idxs :- Matrix
   vec-shuffled :- tsk/Vec]
  (assert (= (count bit-shuffle-idxs) (count vec-shuffled)))
  ; Tried using transient/assoc!/persistent! but was ~10% slower
  (let [init-result (vec (repeat (count bit-shuffle-idxs) ::error1))
        vec-orig    (vec
                      (reduce
                        (fn [cum [icrypt iplain]]
                          (let [tx-val (get vec-shuffled icrypt ::error2)]
                            (assoc cum iplain tx-val)))
                        init-result
                        bit-shuffle-idxs))]
    vec-orig))

;-----------------------------------------------------------------------------
(s/defn ^:no-doc shuffle-bits-BigInteger :- BigInteger
  [ctx :- tsk/KeyMap
   ival :- s/Int]
  (with-map-vals ctx [num-bits bit-shuffle-idxs]
    (it-> ival
      (int->bitchars it num-bits)
      (vec-shuffle bit-shuffle-idxs it)
      (math/binary-chars->BigInteger it))))

(s/defn ^:no-doc unshuffle-bits-BigInteger :- BigInteger
  [ctx :- tsk/KeyMap
   ival :- s/Int]
  ; (prof/with-timer-accum :unshuffle-bits-BigInteger)
  (with-map-vals ctx [num-bits bit-shuffle-idxs]
    (it-> ival
      (int->bitchars it num-bits)
      (vec-unshuffle bit-shuffle-idxs it)
      (math/binary-chars->BigInteger it))))

;-----------------------------------------------------------------------------
(s/defn ^:no-doc encrypt-frame :- BigInteger
  [ctx :- tsk/KeyMap
   ival :- s/Int]
  (with-map-vals ctx [N-max slope offset shuffle-bits?]
    (when-not (and (<= 0 ival) (< ival N-max))
      (throw (ex-info "ival out of range" (vals->map ival N-max))))
    ; calculate mod( y = mx + b ), then shuffle bits
    (let [ival (biginteger ival)
          r1   (it-> ival
                 (.multiply ^BigInteger it slope)
                 (.add ^BigInteger it offset)
                 (mod/mod-BigInteger it N-max))
          r2   (cond-it-> r1
                 shuffle-bits? (shuffle-bits-BigInteger ctx it))]
      r2)))

(s/defn ^:no-doc decrypt-frame :- BigInteger
  [ctx :- tsk/KeyMap
   cuid :- s/Int]
  (with-map-vals ctx [N-max slope slope-inv offset shuffle-bits?]
    (when-not (and (<= 0 cuid) (< cuid N-max))
      (throw (ex-info "cuid out of range" (vals->map cuid N-max))))
    (let [cuid (biginteger cuid)
          r1   (cond-it-> cuid
                 shuffle-bits? (unshuffle-bits-BigInteger ctx it))
          r2   (it-> r1
                 (.subtract ^BigInteger it ^BigInteger offset)
                 (.multiply ^BigInteger it ^BigInteger slope-inv)
                 (mod/mod-BigInteger it N-max))]
      r2)))

;-----------------------------------------------------------------------------
(s/defn ^:no-doc new-ctx-impl :- tsk/KeyMap
  [params :- tsk/KeyMap]
  (with-map-vals params [num-bits rand-seed num-rounds]
    (assert (pos-int? num-bits))
    (assert (pos-int? num-rounds))
    (assert (pos-int? rand-seed))
    (let [random-gen       (Random. rand-seed)

          ; used for text formatting only
          num-digits-hex   (long (Math/ceil (/ num-bits 4))) ; (log2 16) => 4
          num-digits-dec   (long (Math/ceil (/ num-bits (math/log2 10))))

          ; We want slope & offset to be in the central half of all possible values to
          ; "encourage" lots of bits to flip on each multiply/add operation.
          N-max            (math/pow-BigInteger 2 num-bits)
          N-half           (.divide N-max (biginteger 2))
          N-fourth         (.divide N-max (biginteger 4))

          offset           (biginteger
                             (it-> (.nextDouble random-gen)
                               (bigdec it)
                               (* it N-half)
                               (+ it N-fourth)))

          ; ***** MUST BE ODD *****  Thus, it is relatively prime to N-max (power of 2 => even)
          slope            (biginteger
                             (it-> (.nextDouble random-gen)
                               (bigdec it)
                               (* it N-half)
                               (+ it N-fourth)
                               (biginteger it)
                               (if (even? it) ; ensure it is odd
                                   (.add it (biginteger 1))
                                   it)))

          ; compute the "modular inverse" of slope
          slope-inv        (biginteger (mod/modInverse slope N-max))

          ; #todo extract to a function & write unit tests
          ; result is vector of [icrypt iplain] pairs, sorted by icrypt
          bit-shuffle-idxs (let [K                 (Math/round (Math/sqrt num-bits))
                                 bit-idxs-2d       (partition-all K (range num-bits))
                                 bit-idxs-2d-rev   (forv [[irow ibit-seq] (indexed bit-idxs-2d)]
                                                     (if (odd? irow)
                                                       (reverse ibit-seq)
                                                       ibit-seq))
                                 bit-idxs-shuffled (vec (apply interleave-all bit-idxs-2d-rev)) ; example [0 7 8 15 1 6 9 14 2 5 10 13 3 4 11 12]
                                 >>                (assert (= (set (range num-bits)) (set bit-idxs-shuffled)))
                                 bit-idxs-pairs    (indexed bit-idxs-shuffled)]
                             bit-idxs-pairs)]

      ;-----------------------------------------------------------------------------
      ; sanity checks
      (when-not (<= min-bits num-bits max-bits)
        (throw (ex-info "num-bits out of range " (vals->map num-bits min-bits max-bits))))
      (assert (biginteger? offset))
      (assert (biginteger? slope))
      (when-not (odd? slope) ; odd => relatively prime to 2^N
        (throw (ex-info "slope failed even test" (vals->map slope))))

      (when verbose?
        (spyx num-bits)
        (spyx num-digits-hex)
        (spyx N-max)
        (spyx [offset (math/BigInteger->binary-str offset)])
        (spyx [slope (math/BigInteger->binary-str slope)])
        (spyx bit-shuffle-idxs))

      (let [ctx-out (glue params
                      (vals->map num-bits num-rounds num-digits-dec num-digits-hex N-max N-fourth
                        offset slope slope-inv bit-shuffle-idxs))]
        ctx-out))))

(s/defn new-ctx :- tsk/KeyMap
  "Creates a new encryption context map. Usage:

        (new-ctx <params-map>)

  where <params-map> is of the form:

        {:num-bits     <long>  ; REQUIRED:  (minimum: 4): input/output integers in [0..2^n)
         :rand-seed    <long>  ; optional:  encryption key (default: randomized)
         :num-rounds   <long>  ; optional:  positive int (default: 2)
        } "
  [opts :- tsk/KeyMap]
  (s/validate {:num-bits                       s/Int
               (s/optional-key :rand-seed)     s/Int
               (s/optional-key :num-rounds)    s/Int
               (s/optional-key :shuffle-bits?) s/Bool}
    opts)
  (let [opts-default {:rand-seed     (Math/abs (.nextLong (Random.))) ; positive for simplicity
                      :num-rounds    3
                      :shuffle-bits? true}
        ctx          (new-ctx-impl (glue opts-default opts))]
    ctx))

;-----------------------------------------------------------------------------
; Timing {:num-rounds 5  :shuffle-bits? false}
;   32 bits:  10 usec/call
;   64 bits:  10 usec/call
;  128 bits:  10 usec/call
;  256 bits:  11 usec/call
;
; Timing {:num-rounds 2  :shuffle-bits? true}
;   32 bits:  25 usec/call
;   64 bits:  45 usec/call
;  128 bits:  80 usec/call
;  256 bits: 157 usec/call
(s/defn encrypt :- BigInteger
  [ctx :- tsk/KeyMap
   ival :- s/Int]
  ; (prof/with-timer-accum :idx->cuid)
  (iterate-n (grab :num-rounds ctx)
    #(encrypt-frame ctx %)
    (biginteger ival)))

(s/defn decrypt :- BigInteger
  [ctx :- tsk/KeyMap
   cuid :- s/Int]
  ; (prof/with-timer-accum :cuid->idx)
  (iterate-n (grab :num-rounds ctx)
    #(decrypt-frame ctx %)
    (biginteger cuid)))

