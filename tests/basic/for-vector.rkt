#lang racket

(define (fn vec)
  (for ([v vec])
    (displayln v)))

(fn #(1 2 3 4 5))
