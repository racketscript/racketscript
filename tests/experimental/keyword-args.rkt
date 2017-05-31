#lang racket

(define (foo #:a a #:b b)
  (displayln a)
  (displayln b))

(foo #:a 'hello #:b 'world)
(foo #:b 'world #:a 'hello)
