#lang racket

(define-syntax (swap stx)
  (syntax-case stx ()
    [(_ a b)
     #'(let ([tmp a])
         (set! a b)
         (set! b tmp))]))

(define a 10)
(define b 12)
(displayln a)
(displayln b)
(displayln "")
(swap a b)
(displayln a)
(displayln b)

