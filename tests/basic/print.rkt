#lang racket/base

(println '(1 2 3))
(parameterize ([print-as-expression #f])
  (println '(1 2 3)))
(println '(1 2 3) (current-output-port) 1)

((current-print) '(1 2 3))
((current-print) (void)) ;;TODO

;; only println should have starting '
(displayln '(1 2 3 4))
(writeln '(1 2 3 4))
(println '(1 2 3 4))
