#lang racket/base

(provide factorial factorial/tail)

(define (factorial n)
  (cond
    [(zero? n) 1]
    [else (* n (factorial (sub1 n)))]))

(define (factorial/tail n a)
  (cond
    [(zero? n) a]
    [else (factorial/tail (sub1 n) (* n a))]))


(displayln (factorial 0))
(displayln (factorial 5))
(displayln (factorial 6))
(displayln (factorial/tail 0 1))
(displayln (factorial/tail 5 1))
(displayln (factorial/tail 6 1))
