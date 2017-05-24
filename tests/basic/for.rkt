#lang racket

(define (fn lst)
  (for ([v lst])
    (displayln v)))

(fn (list 1 2 3 4))
