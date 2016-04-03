#lang racket/base

(struct posn (x y) #:transparent)

(displayln (posn 1 2))

