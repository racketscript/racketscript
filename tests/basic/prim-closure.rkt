#lang racket

(define (close)
  (define a 0)
  (lambda ()
    (set! a (add1 a))
    (displayln a)))

(define f (close))
(f)
(f)
(f)

(define g (close))
(g)
(g)
(g)
