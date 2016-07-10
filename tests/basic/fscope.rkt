#lang racket

(define (foo a b)
  (define (bar a)
    (* a a ))
  (+ (bar b) a))

(displayln (foo 4 5))
    

