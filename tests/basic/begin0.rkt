#lang racket

(define x 0)

(begin0 x
        (set! x (add1 x))
        (set! x (add1 x))
        (add1 x))
