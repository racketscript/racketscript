#lang racketscript/boot

(require racketscript/interop "lib.rkt" "syntax.rkt")

(define+provide (variable-reference-from-unsafe? v) #false)
(define+provide (variable-reference-constant? v) #false)

(define-syntax-rule (bounce ids ...)
  (#js.Core.Hash.makeEqual
   (array (array 'ids ids) ...)
   #false))

;; one big table with everything
(define+provide (primitive-table v)
  (bounce syntax? syntax-e datum->syntax syntax->datum
          syntax-property syntax-property-symbol-keys
          syntax-source syntax-line syntax-column syntax-span
          syntax-position
          variable-reference-constant?
          variable-reference-from-unsafe?))
  
