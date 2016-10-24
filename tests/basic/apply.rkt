#lang racket

(apply (lambda () 42) '())
(apply + '(1 2 3 4 5 6 7))
(apply + 1 2 3 '(4 5 6 7))
