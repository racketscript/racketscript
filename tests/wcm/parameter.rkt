#lang racket

(define par (make-parameter 0))

(define (foo)
  (displayln (par)))

(parameterize ([par 1])
  (displayln (par))
  (parameterize ([par 2])
    (foo))
  (foo))
