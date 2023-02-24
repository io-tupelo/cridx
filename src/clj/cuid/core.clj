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
(def ^:no-doc max-bits 128) ; No real upper limit.  Just process in blocks if desired.

;-----------------------------------------------------------------------------

