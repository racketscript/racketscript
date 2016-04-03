#lang racket/base
;#lang typed/racket

(require racket/format
         "util.rkt")

(provide output-directory
         module-output-file
         BASE-ENV)

(define output-directory (make-parameter "js-build"))

(define KERNEL-MODULE-NAME "__$RACKETKERNEL")

(define (add-import e p s)
  (hash-set e s (string->symbol (~a p "." (normalize-symbol s)))))

(define (add-kernel-import s e)
  (add-import e KERNEL-MODULE-NAME s))

(define (add-kernel-imports* e s*)
  (foldl add-kernel-import e s*))

(define CORE-IMPORTS
  ;; TODO: Many of these could be represented using JS operators
  (hash ;; (inst hash Symbol Symbol)
   '* '__$RACKETCORE.Number.multiply
   '+ '__$RACKETCORE.Number.add
   '- '__$RACKETCORE.Number.subtract
   '/ '__$RACKETCORE.Number.divide
   '< '__$RACKETCORE.Number.lt
   '> '__$RACKETCORE.Number.gt
   '<= '__$RACKETCORE.Number.lte
   '>= '__$RACKETCORE.Number.gte
   '= '__$RACKETCORE.Number.equal))
  
;(: BASE-ENV (HashTable Symbol Symbol))
(define BASE-ENV
  (add-kernel-imports*
   CORE-IMPORTS
   '(zero?
     car
     cdr
     list
     first
     rest
     sub1
     add1
     displayln
     equal?
     values
     call-with-values
     not
     empty?
     print-values
     cons
     null?
     empty?

     ~a
     string-append
     display
     )))

(define (module-output-file mod)
  (cond
    [(or (string? mod) (symbol? mod))
     (build-path (output-directory) "modules" (~a mod ".js"))]
    [else (error "module names are either string or symbol")]))
