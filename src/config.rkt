#lang racket/base
;#lang typed/racket

(require racket/format)

(provide output-directory
         module-output-file
         BASE-ENV)

(define output-directory (make-parameter "js-build"))

;(: BASE-ENV (HashTable Symbol Symbol))
(define BASE-ENV (hash ;; (inst hash Symbol Symbol)
                  '* '__$RACKETCORE.Number.multiply
                  '+ '__$RACKETCORE.Number.add
                  '- '__$RACKETCORE.Number.subtract
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

(define (module-output-file mod)
  (cond
    [(or (string? mod) (symbol? mod))
     (build-path (output-directory) "modules" (~a mod ".js"))]
    [else (error "module names are either string or symbol")]))
