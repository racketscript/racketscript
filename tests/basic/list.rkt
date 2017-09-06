#lang racket/base

(define (list-sq)
  (define lst '(1 2 3 4 5))
  (map (lambda (x) (* x x)) lst))

(displayln (list-sq))
(displayln (length (list-sq)))

(equal? '(1 2 3) '(1 2 3))
(equal? (list 1 2 3) '(1 2 3))

(eq? '() null)
(null? '())
(list? '())
(list? (cons 1 2))
(list? (cons 1 '()))
(list? (cons 1 (cons 2 '())))
(equal? (cons 1 '()) (list 1))

(pair? '())
(pair? (cons 1 #\x))

(immutable? '())
