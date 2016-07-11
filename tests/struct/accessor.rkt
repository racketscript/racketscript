#lang racket/base

(struct posn (x y))
(struct circle (center radius))

(define a (posn 1 2))
(define b (posn 3 8))

(equal? (posn-x a) 1)
(equal? (posn-y a) 2)
(equal? (posn-x b) 3)
(equal? (posn-y b) 8)
