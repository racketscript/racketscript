#lang racket/base

(println '(1 2 3))
(print-as-expression #f)
(println '(1 2 3))
(print-as-expression #t)
(println '(1 2 3) (current-output-port) 1)

((current-print) '(1 2 3))
((current-print) (void))
