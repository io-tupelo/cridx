(ns tst.cuid.core
  (:use cuid.core
        tupelo.core
        tupelo.test)
  (:require
    [com.climate.claypoole :as cp]
    [tupelo.string :as str]
    ))

(set! *warn-on-reflection* true)

(comment

; Fast coverage tests
(doseq [nbits (thru 4 9)]
  (let [ctx (new-ctx {:num-bits nbits})]
    (with-map-vals ctx [N-max]
      (let [idx-vals    (range N-max)
            cuid-vals   (cp/pmap :builtin #(idx->cuid ctx %) idx-vals)
            idx-decrypt (cp/pmap :builtin #(cuid->idx ctx %) cuid-vals)]
        (is-set= idx-vals cuid-vals) ; all vals present
        ; (spyx idx-vals )
        ; (spyx cuid-vals)
        (isnt= idx-vals cuid-vals) ; but not same order (random chance 1 in N!)
        (is= idx-vals idx-decrypt) ; decryption recovers original vals, in order
        ))))

  )
