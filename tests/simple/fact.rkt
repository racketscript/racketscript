#lang racket

(require racket/base)

(provide factorial)

(define (factorial n)
  (cond
    [(zero? n) 1]
    [else (* n (factorial (sub1 n)))]))

(define (factorial/tail n a)
  (cond
    [(zero? n) a]
    [else (factorial/tail (sub1 n) (* n a))]))


(displayln (factorial 5))
(displayln (factorial/tail 5 1))
