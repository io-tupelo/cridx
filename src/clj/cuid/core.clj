(ns cuid.core
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [cuid.modular-arithmetic :as modmath]
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

(comment


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


  )
