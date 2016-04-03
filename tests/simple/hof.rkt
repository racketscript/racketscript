#lang racket/base


(define (map fn lst)
  (cond
    [(null? lst) '()]
    [else (cons (fn (car lst))
                (map fn (cdr lst)))]))

(define lst '(1 2 3 4))

(displayln lst)
(displayln (map (lambda (x) (* x x)) lst))
(displayln lst)

