#lang racketscript/boot

(require racketscript/interop
         "lib.rkt")

;; TODO: why does this need to be here and in kernel.rkt?
(define+provide values
  (v-λ vals
    (if (binop === #js.vals.length 1)
        ($ vals 0)
        (#js.Values.make vals))))
