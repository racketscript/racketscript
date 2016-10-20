#lang racket

(define (factorial n)
  (let loop ([n n]
             [a 1])
    (if (zero? n)
      a
      (loop (sub1 n) (* n a)))))

(define cc0 (compose factorial sqr))
(define cc1 (compose1 factorial sqr))
(define cc2 (compose sqr factorial))
(define cc3 (compose1 sqr factorial))

(displayln (cc0 3))
(displayln (cc1 3))
(displayln (cc2 3))
(displayln (cc3 3))
