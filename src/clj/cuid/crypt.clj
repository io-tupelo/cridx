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

(s/defn crypt
  [N m x]
  (mod/mult-mod-Long m x N))

(s/defn decrypt
  [N m c]
  (let [minv   (mod/modInverse m N)
        result (mod/mult-mod-Long c minv N)]
    result))

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
  (prof/with-timer-accum :shuffle-bits-BigInteger
    (with-map-vals ctx [num-bits bit-shuffle-idxs]
      (it-> ival
        (prof/with-timer-accum :shuffle-bits-BigInteger-a
          (int->bitchars it num-bits))
        (prof/with-timer-accum :shuffle-bits-BigInteger-b
          (vec-shuffle bit-shuffle-idxs it))
        (prof/with-timer-accum :shuffle-bits-BigInteger-c
          (math/binary-chars->BigInteger it))))))

(s/defn ^:no-doc unshuffle-bits-BigInteger :- BigInteger
  [ctx :- tsk/KeyMap
   ival :- s/Int]
  (prof/with-timer-accum :unshuffle-bits-BigInteger
    (with-map-vals ctx [num-bits bit-shuffle-idxs]
      (it-> ival
        (prof/with-timer-accum :unshuffle-bits-BigInteger-a
          (int->bitchars it num-bits))
        (prof/with-timer-accum :unshuffle-bits-BigInteger-b
          (vec-unshuffle bit-shuffle-idxs it))
        (prof/with-timer-accum :unshuffle-bits-BigInteger-c
          (math/binary-chars->BigInteger it))))))

;-----------------------------------------------------------------------------
(s/defn ^:no-doc encrypt-frame :- BigInteger
  [ctx :- tsk/KeyMap
   ival :- s/Int]
  (prof/with-timer-accum :encrypt-frame
    (with-map-vals ctx [N-max slope offset]
      (when-not (and (<= 0 ival) (< ival N-max))
        (throw (ex-info "ival out of range" (vals->map ival N-max))))
      ; calculate mod( y = mx + b ), then shuffle bits
      (let ; -spy-pretty
        [r1 (it-> (biginteger ival)
              (.multiply ^BigInteger it slope)
              (.add ^BigInteger it offset)
              (mod/mod-BigInteger it N-max))
         r2 (cond-it-> r1
              (:shuffle-bits? ctx) (shuffle-bits-BigInteger ctx it))]
        r1))))

(s/defn ^:no-doc decrypt-frame :- BigInteger
  [ctx :- tsk/KeyMap
   cuid :- s/Int]
  (prof/with-timer-accum :decrypt-frame
    (with-map-vals ctx [N-max slope slope-inv offset]
      (when-not (and (<= 0 cuid) (< cuid N-max))
        (throw (ex-info "cuid out of range" (vals->map cuid N-max))))
      (let [r1 (cond-it-> (biginteger cuid)
                 (:shuffle-bits? ctx) (unshuffle-bits-BigInteger ctx it))
            r2 (it-> r1
                 (.subtract ^BigInteger it ^BigInteger offset)
                 (.multiply ^BigInteger it ^BigInteger slope-inv)
                 (mod/mod-BigInteger it N-max))]
        r2))))

;-----------------------------------------------------------------------------
(s/defn ^:no-doc new-ctx-impl :- tsk/KeyMap
  [params :- tsk/KeyMap]
  (with-map-vals params [num-bits rand-seed num-rounds verbose?]
    (assert (pos-int? num-bits))
    (assert (pos-int? num-rounds))
    (assert (pos-int? rand-seed))
    (let  ; -spy
      [random-gen       (Random. rand-seed)

       ; used for text formatting only
       num-digits-hex   (long (Math/ceil (/ num-bits 4))) ; (log2 16) => 4
       num-digits-dec   (long (Math/ceil (/ num-bits (math/log2 10))))

       ; We want slope & offset to be in the central third of all possible values to
       ; "encourage" lots of bits to flip on each multiply/add operation.
       N-max            (math/pow-BigInteger 2 num-bits)
       N-third          (.divide N-max (biginteger 3))

       offset           (biginteger
                          (it-> (.nextDouble random-gen)
                            (* it N-third)
                            (+ it N-third)))

       ; ***** MUST BE ODD *****  Thus, it is relatively prime to N-max (power of 2 => even)
       slope            (biginteger
                          (it-> (.nextDouble random-gen)
                            (* it N-third)
                            (+ it N-third)
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
                      (vals->map num-bits num-rounds num-digits-dec num-digits-hex N-max N-third
                        offset slope slope-inv bit-shuffle-idxs))]
        ctx-out))))

(s/defn new-ctx :- tsk/KeyMap
  "Creates a new CUID context map. Usage:

        (new-ctx <params-map>)

  where <params-map> is of the form:

        {:num-bits     <long>  ; REQUIRED:  (minimum: 4): input/output integers in [0..2^n)
         :rand-seed    <long>  ; optional:  encryption key (default: randomized)
         :num-rounds   <long>  ; optional:  positive int (default: 2)
         :verbose?     false   ; optional:  enable for dbg prints (default: false)
        } "
  [opts :- tsk/KeyMap]
  (s/validate {:num-bits                       s/Int
               (s/optional-key :rand-seed)     s/Int
               (s/optional-key :num-rounds)    s/Int
               (s/optional-key :verbose?)      s/Bool
               (s/optional-key :shuffle-bits?) s/Bool}
    opts)
  (let [params-default {:rand-seed     (Math/abs (.nextLong (Random.))) ; positive for simplicity
                        :num-rounds    5
                        :verbose?      false
                        :shuffle-bits? false} ; timing is about 100x slower when enabled
        params         (glue params-default opts)
        ctx            (new-ctx-impl params)]
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
(s/defn idx->cuid :- BigInteger
  [ctx :- tsk/KeyMap
   ival :- s/Int]
  (prof/with-timer-accum :idx->cuid
    (nth
      (iterate #(encrypt-frame ctx %) ival) ; NOTE: seq is [x  (f x)  (f (f x))...] so don't use (dec N)
      (grab :num-rounds ctx))))

(s/defn cuid->idx :- BigInteger
  [ctx :- tsk/KeyMap
   cuid :- s/Int]
  (prof/with-timer-accum :cuid->idx
    (nth
      (iterate #(decrypt-frame ctx %) cuid)
      (grab :num-rounds ctx))))

