(ns cuid.core
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [cuid.crypt :as crypt]
    [tupelo.profile :as prof]
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
(def ^:no-doc max-bits 128) ; No real upper limit.  Just process in blocks if desired.

;-----------------------------------------------------------------------------

(s/defn new-ctx :- tsk/KeyMap
  "Creates a new CUID context map. Usage:

        (new-ctx <params-map>)

  where <params-map> is of the form:

        {:num-bits     <long>  ; REQUIRED:  (minimum: 4): input/output integers in [0..2^n)
         :rand-seed    <long>  ; optional:  encryption key (default: randomized)
         :num-rounds   <long>  ; optional:  positive int (default: 5)
        } "
  [opts :- tsk/KeyMap]
  (s/validate {:num-bits                       s/Int
               (s/optional-key :rand-seed)     s/Int
               (s/optional-key :num-rounds)    s/Int}
    opts)

  (let [num-bits (:num-bits opts)]
    (when num-bits
      (when-not (<= min-bits num-bits max-bits)
        (throw (ex-info "num-bits out of range " (vals->map num-bits min-bits max-bits))))))
  (let [params-default {:rand-seed     (Math/abs (.nextLong (Random.))) ; positive for simplicity
                        :num-rounds    7
                        :shuffle-bits? false}
        ctx            (crypt/new-ctx (glue params-default opts))]
    ctx))

;-----------------------------------------------------------------------------
; Timing {:num-rounds 5  :shuffle-bits? false}
;   32 bits:  10 usec/call
;   64 bits:  10 usec/call
;  128 bits:  10 usec/call
;  256 bits:  11 usec/call
(s/defn idx->cuid :- BigInteger
  [ctx :- tsk/KeyMap
   ival :- s/Int]
  (crypt/encrypt ctx ival ))

(s/defn cuid->idx :- BigInteger
  [ctx :- tsk/KeyMap
   cuid :- s/Int]
  (crypt/decrypt ctx cuid))
