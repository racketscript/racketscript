#lang racket

(define (is-odd? n)
  (letrec ([is-even? (lambda (n)
                       (or (zero? n)
                           (is-odd? (sub1 n))))]
           [is-odd? (lambda (n)
                      (and (not (zero? n))
                           (is-even? (sub1 n))))])
    (is-odd? n)))


(displayln (is-odd? 10))
(displayln (is-odd? 12))
(displayln (is-odd? 15))
(displayln (is-odd? 17))
