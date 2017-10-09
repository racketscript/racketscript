#lang racket

(struct a+1 (v)
  #:guard (λ (v name)
            (displayln (list v))
            (add1 v)))

(define a1 (a+1 0))
(displayln (a+1-v a1))
(define a2 (a+1 1))
(displayln (a+1-v a2))
