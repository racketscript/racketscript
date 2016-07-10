#lang racket

(define (even? n)
  (if (zero? n)
    #t
    (odd? (sub1 n))))

(define (odd? n)
  (if (zero? n)
    #f
    (even? (sub1 n))))


(displayln (even? 10))
(displayln (odd? 12))
(displayln (odd? 13))
(displayln (even? 17))

