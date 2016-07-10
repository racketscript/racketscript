#lang racket/base

(require "lib.rkt")

(define add
  (λ vs
    (foldl + 0 vs)))

(define add2
  (λ (a b . c)
    (* (foldl + 0 c) a b)))

(displayln (add 1 2 3 4 5))
(displayln (add2 1 2 3 4 5))
