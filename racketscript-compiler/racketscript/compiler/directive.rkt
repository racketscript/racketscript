
#lang racket/base

(require (for-syntax racket
                     syntax/parse))

(provide #%rs-compiler
         if-scheme-numbers)

;; #%rs-compiler is treated specially by the compiler to implement
;; compiler directives.
(define #%rs-compiler
  (lambda _
    (#%app error 'racketscript "cannot use Racketscript compiler directive in Racket")))

(define-syntax (if-scheme-numbers stx)
  (syntax-parse stx
    [(_ consequent:expr alternate:expr)
     #'(#%rs-compiler 'if-scheme-numbers
           consequent
           alternate)]
    [(_ consequent:expr)
     #'(#%rs-compiler 'if-scheme-numbers
                      consequent)]))
