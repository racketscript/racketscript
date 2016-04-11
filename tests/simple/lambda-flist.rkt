#lang racket/base

(define add
  (λ vs
    (foldr + 0 vs)))

(displayln (add 1 2 3 4 5))

