#lang racket/base

(struct posn (x y))
(struct circle (center radius))

(define a (posn 1 2))
(define b (posn 3 8))

(circle? a)
(circle? b)

(posn? a)
(posn? b)

(posn? (circle (posn 1 2) 12))
(circle? (circle (posn 1 2) 12))
