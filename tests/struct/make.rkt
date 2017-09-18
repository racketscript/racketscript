#lang racket

(provide (struct-out posn)
         posn0)

(struct posn (x y))
(struct posn0 (x y) #:transparent)

(define pa (posn 0 1))
(displayln (posn-x pa))
(displayln (posn-y pa))

(define pb (posn0 1 2))
(equal? (posn0 1 2) pb)
(displayln pb)
(writeln pb)
(println pb)

(displayln (posn '(1 2) #\x))
(writeln (posn '(1 2) #\x))
(println (posn '(1 2) #\x))

(displayln (posn0 '(1 2) #\x))
(writeln (posn0 '(1 2) #\x))
(println (posn0 '(1 2) #\x))
