#lang racket/base

(define (list-sq)
  (define lst '(1 2 3 4 5))
  (map (lambda (x) (* x x)) lst))

(displayln (list-sq))
(displayln (length (list-sq)))

(equal? '(1 2 3) '(1 2 3))
