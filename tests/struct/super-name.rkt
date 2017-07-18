#lang racket

(struct posn (x y)     #:guard (λ (x y name)   (displayln (list '2d x y name))   (values x y)))
(struct posn3 posn (z) #:guard (λ (x y z name) (displayln (list '3d x y z name)) (values x y z)))
(define p1 (posn3 10 12 14))
(displayln (list (posn-x p1)
                 (posn-y p1)
                 (posn3-z p1)))

