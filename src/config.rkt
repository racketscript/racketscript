#lang racket/base
;#lang typed/racket

(provide output-directory
         BASE-ENV)

(define output-directory (make-parameter "js-build"))

;(: BASE-ENV (HashTable Symbol Symbol))
(define BASE-ENV (hash ;; (inst hash Symbol Symbol)
                  '* '__$RACKETCORE.Number.multiply
                  '+ '__$RACKETKCORE.Number.add
                  '- '__$RACKETKCORE.Number.subtract
                  '/ '__$RACKETCORE.Number.divide
                  'zero? '__$RACKETKERNEL.zero_p
                  'car '__$RACKETKERNEL.car
                  'cdr '__$RACKETKERNEL.cdr
                  'list '__$RACKETKERNEL.list
                  'first '__$RACKETKERNEL.first
                  'rest '__$RACKETKERNEL.rest
                  'sub1 '__$RACKETKERNEL.sub1
                  'add1 '__$RACKETKERNEL.add1
                  'displayln '__$RACKETKERNEL.displayln
                  'equal? '__$RACKETKERNEL.equal_p
                  'values '__$RACKETKERNEL.values
                  'call-with-values '__$RACKETKERNEL.call_with_values
                  'not '__$RACKETKERNEL.bnot
                  'empty? '__$RACKETKERNEL.empty_p
                  'print-values '__$RACKETKERNEL.print_values
                  ))
