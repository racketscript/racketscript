#lang racketscript/base

(define x 0)

(define (change-x)
  (set! x (add1 x))
  "var changed")

(define (foo a b)
  (displayln (list a b)))

(foo (change-x) x)
(foo x (change-x))
