#lang racket

(struct posn (x y)
  #:guard (λ (x y name)
            (displayln (list x y))
            (values x y)))

(define p1 (posn 10 12))
(define p2 (posn 15 23))
(posn-x p1)
(posn-y p1)
(posn-x p2)
(posn-y p2)
