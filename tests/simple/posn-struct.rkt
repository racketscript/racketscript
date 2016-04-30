#lang racket/base

(struct posn (x y))
(struct foo ())

(displayln (posn 1 2))
(define pt (posn 4 5))
(displayln (posn-x pt))
(displayln (posn-y pt))
(displayln (posn? pt))
(displayln (posn? (foo)))

