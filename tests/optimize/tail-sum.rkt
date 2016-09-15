#lang racket

(define (sum n a)
  (if (zero? n)
    a
    (sum (sub1 n) (+ a n))))

(sum 100 0)
(sum 1000 0)
(sum 10000 0)

