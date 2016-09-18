#lang racket/base

(define (loop n)
  (cond
    [(> n 10000)
     (if (zero? n)
       1
       (loop (sub1 n)))]
    [else 0]))

(loop 12000)
