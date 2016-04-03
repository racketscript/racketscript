#lang racket

(require racket/base)

(provide factorial)

(define (factorial n)
  (cond
    [(zero? n) 1]
    [else (* n (factorial (sub1 n)))]))


(displayln (factorial 5))
