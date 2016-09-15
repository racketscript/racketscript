#lang racket

(define (foo n . c)
  (if (zero? n)
    1
    (foo (sub1 n) 0 1 2)))

(foo 100 0 2 3)
