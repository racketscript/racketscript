#lang racket/base

(struct posn (x y))
(struct circle (center radius))

(define a (posn 1 2))
(define b (posn 3 8))
(define c1 (circle a 20))
(define c2 (circle b 24))

(equal? a (posn 3 4))
(equal? b (posn 3 8))
(equal? b (posn 1 2))

(equal? a b)

(equal? c1 c2)
(equal? c1 (circle (posn 1 2)  20))
(equal? c2 (circle (posn 3 8)  24))
