#lang racket

(displayln (car '(1 . 2)))
(displayln (cdr '(1 . 2)))

(displayln '((+ 3 4) . 1))
(displayln (car '((+ 3 4) . 1)))
(displayln (cdr '((+ 3 4) . 1)))
