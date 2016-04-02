#lang racket/base
;#lang typed/racket

(provide (all-defined-out))

(define output-directory (make-parameter "js-build"))

;(: BASE-ENV (HashTable Symbol Symbol))
(define BASE-ENV (hash ;; (inst hash Symbol Symbol)
                  '* '__$RACKETKERNEL.Number.multiply
                  '+ '__$RACKETKERNEL.Number.add
                  '- '__$RACKETKERNEL.Numbe.subtract
                  '/ '__$RACKETKERNEL.Numbe.divide))
