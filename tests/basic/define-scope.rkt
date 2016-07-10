#lang racket

(define (hey n)
  (displayln "called hey upper"))

(define (foo n)
  (define (hey n)
    (displayln "called hey inside"))
  (define (bar n)
    (hey 2)
    (displayln "called hey inside"))
  (bar 2))

(define (foos n)
  (define (bar n)
    (hey 2)
    (displayln "called hey inside"))
  (define (hey n)
    (displayln "called hey inside"))
  (bar 1))

(displayln (foo 1))
