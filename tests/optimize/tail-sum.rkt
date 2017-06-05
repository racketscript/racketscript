#lang racket

(define (sum n a)
  (if (zero? n)
    a
    (sum (sub1 n) (+ a n))))

(displayln (sum 100 0))
(displayln (sum 1000 0))
(displayln (sum 10000 0))
;;(displayln (sum 15000 0))

