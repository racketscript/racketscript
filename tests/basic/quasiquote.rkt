#lang racket

(displayln `(1 2 3 (4 5 6) ,(+ 7 8 9)))
(displayln `(1 2 3 (4 5 6) ,(+ 7 8 9) ,@(list 10 11 12)))
(displayln `(1 2 3 (4 [5 6]) ,(+ 7 8 9) ,@(list 10 11 12)))
