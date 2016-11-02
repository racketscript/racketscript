#lang racket

(require "make.rkt")

(define pa (posn 0 1))
(displayln (posn-x pa))
(displayln (posn-y pa))

(define pb (posn0 1 2))
(equal? (posn0 1 2) pb)
(displayln pb)
