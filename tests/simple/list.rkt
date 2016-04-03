#lang racket

(define (list-sq)
  (define lst '(1 2 3 4 5))
  (map (lambda (x) (* x x)) lst))
