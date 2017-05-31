#lang racket

(define (foo #:a a #:b b)
  (displayln a)
  (displayln b))

(foo #:a 'hello #:b 'world)
(foo #:b 'world #:a 'hello)

(define (bar a b #:c c #:d d)
  (displayln (list a b c d)))
(bar 1 2 #:c 3 #:d 4)
(bar 1 2 #:d 3 #:c 4)
