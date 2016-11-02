#lang racket

(provide (struct-out posn)
         posn0)

(struct posn (x y))
(struct posn0 (x y) #:transparent)
