(ns cuid.core
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [cuid.modular-arithmetic :as modmath ]
    [tupelo.math :as math]
    [tupelo.schema :as tsk]
    [tupelo.string :as str])
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
(def Matrix [[s/Any]])

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

;-----------------------------------------------------------------------------
(defn modInverse
  "Computes the 'inverse` y of a number x (mod N), such that `x*y (mod N)` = 1.
  Uses the extended Euclid algorithm (iterative version). Assumes x and N are relatively prime. "
  [x N]
  (assert (and (pos? x) (pos? N) (< x N)))
  (let [N-orig N
        a      1
        b      0]
    (if (= 1 N)
      (throw (ex-info "Invalid N" (vals->map x N)))
      (loop [x x
             n N
             a a
             b b]
        (if (< 1 x)
          (let [x-next n
                N-next (mod x n)
                q      (quot x n)
                a-next b
                b-next (- a (* q b))]
            (recur x-next N-next a-next b-next))
          (if (neg? a)
            (+ a N-orig)
            a))))))

(s/defn add-mod :- s/Int
  "Adds two numbers a and b (mod N)."
  [a :- s/Int
   b :- s/Int
   N :- s/Int]
  (assert (and (pos? N) (< 1 N)))
  (it-> (+ a b)
    (mod it N)))

(s/defn mult-mod :- s/Int
  "Multiply two numbers a and b (mod N)."
  [a :- s/Int
   b :- s/Int
   N :- s/Int]
  (assert (and (pos? N) (< 1 N)))
  (it-> (* a b)
    (mod it N)))

(s/defn crypt
  [N m x]
  (mult-mod m x N))

(s/defn decrypt
  [N m c]
  (let [; decrypt
        inv-val (modInverse m N)
        result  (it-> (mult-mod c inv-val N))]
    result))


#_(let    ; -spy
    [

     N-ovr-2 (quot N 2)
     flip?   (< N-ovr-2 m)

     [ymod m] (if flip?
                [(- N y)
                 (- N m)]
                [y m])

     Q       (floor-long (/ (double N) m))
     ymax    (* Q m)
     short   (- N ymax)
     low     (mod ymod m)
     lraw    (if (pos? low)
               (- m low)
               low)
     l       (quot lraw short)
     y       (mod (+ ymod (* N l)) (* m N))
     x       (quot y m)
     ]
    (spyx (vals->map ymax short y low lraw l y x)))

;-----------------------------------------------------------------------------
(s/defn ^:no-doc new-ctx-impl :- tsk/KeyMap
  [params :- tsk/KeyMap]
  (with-map-vals params [num-bits rand-seed num-rounds verbose]
    (assert (pos-int? num-bits))
    (assert (pos-int? num-rounds))
    (assert (pos-int? rand-seed))
    (let-spy
      [random-gen     (Random. rand-seed)

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
       ; result is vector of [icrypt iplain] pairs, sorted by icrypt
       ibit-tx-orig   (let [K             (Math/round (Math/sqrt num-bits))
                            ibit-seqs     (partition-all K (range num-bits))
                            ibit-seqs-rev (forv [[i ibit-seq] (indexed ibit-seqs)]
                                            (if (odd? i)
                                              (decrypt ibit-seq)
                                              ibit-seq))
                            ibit-shuffled (vec (apply interleave-all ibit-seqs-rev)) ; example [0 7 8 15 1 6 9 14 2 5 10 13 3 4 11 12]
                            itx->isrc     (indexed ibit-shuffled)]
                        (assert (= (set (range num-bits)) (set ibit-shuffled)))
                        itx->isrc)

       [Q short] (.divideAndRemainder N-max slope)
       ]

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
        (spyx ibit-tx-orig))

      (vals->map num-bits num-rounds num-dec-digits num-hex-digits N-max N-third
        offset slope ibit-tx-orig Q short
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
(s/defn ^:no-doc vec-shuffle :- tsk/Vec ; #todo: maybe make more general version?
  [ibit-tx-orig :- Matrix
   vec-orig :- tsk/Vec]
  (assert (= (count ibit-tx-orig) (count vec-orig)))
  ; Since `ibit-tx-orig` is sorted by icrypt, we can just go in sequence
  (let [vec-shuffled (forv [[icrypt iplain] ibit-tx-orig]
                       (get vec-orig iplain ::error))]
    vec-shuffled))

(s/defn ^:no-doc vec-unshuffle :- tsk/Vec
  [ibit-tx-orig :- Matrix
   vec-shuffled :- tsk/Vec]
  (assert (= (count ibit-tx-orig) (count vec-shuffled)))

  ; Use of transient/assoc!/persistent! may only be a 2-5x speedup, may not be worth keeping
  ; See:  https://tech.redplanetlabs.com/2020/09/02/clojure-faster/#clojure-transients
  ;       https://clojure.org/reference/transients
  (let [init-result (transient
                      (vec (repeat (count ibit-tx-orig) ::error1)))
        vec-orig    (vec
                      (persistent!
                        (reduce
                          (fn [cum [icrypt iplain]]
                            (let [tx-val (get vec-shuffled icrypt ::error2)]
                              (assoc! cum iplain tx-val)))
                          init-result
                          ibit-tx-orig)))]
    vec-orig))

(s/defn ^:no-doc shuffle-int-bits :- BigInteger
  [ctx :- tsk/KeyMap
   ival :- s/Int]
  (with-map-vals ctx [num-bits ibit-tx-orig]
    (it-> ival
      (int->bitchars it num-bits)
      (vec-shuffle ibit-tx-orig it)
      (math/binary-chars->BigInteger it))))

(s/defn ^:no-doc unshuffle-int-bits :- BigInteger
  [ctx :- tsk/KeyMap
   ival :- s/Int]
  (with-map-vals ctx [num-bits ibit-tx-orig]
    (it-> ival
      (int->bitchars it num-bits)
      (vec-unshuffle ibit-tx-orig it)
      (math/binary-chars->BigInteger it))))

(s/defn ^:no-doc encrypt-frame :- BigInteger
  [ctx :- tsk/KeyMap
   ival :- s/Int]
  (prn :-----------------------------------------------------------------------------)
  (with-map-vals ctx [N-max slope offset]
    #_(when-not (and (<= 0 ival) (< ival N-max))
        (throw (ex-info "ival out of range" (vals->map ival N-max))))
    ; calculate mod( y = mx + b ), then shuffle bits
    (let-spy-pretty
      [ival   (biginteger ival)
       mx     (.multiply ^BigInteger ival slope)
       y      (.add ^BigInteger offset mx)
       ymod   (.mod ^BigInteger y N-max)
       result (shuffle-int-bits ctx ymod)
       ]
      (biginteger result))))

(def ^:no-doc bi-0 (biginteger 0))
(def ^:no-doc bi-1 (biginteger 1))


(s/defn ^:no-doc decrypt-frame :- BigInteger
  [ctx :- tsk/KeyMap
   cuid :- s/Int]
  (prn :-----------------------------------------------------------------------------)
  (with-map-vals ctx [N-max slope offset Q short]
    #_(when-not (and (<= 0 cuid) (< cuid N-max))
        (throw (ex-info "cuid out of range" (vals->map cuid N-max))))
    (let-spy-pretty
      [slope-sym (modmath/mod-symmetric slope N-max)

       ymod      (unshuffle-int-bits ctx cuid)
       yprime    (modmath/mod-symmetric (- ymod offset) N-max)

       ]
      ;  (biginteger x)
      nil
      )))

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

