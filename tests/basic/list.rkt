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

(displayln (cons 1 #\x))
(writeln (cons 1 #\x))
(println (cons 1 #\x))

(displayln '(1 2 3))
(displayln '())
(writeln '(1 2 3))
(writeln '())
(println '(1 2 3))
(println '(1 2 3) (current-output-port) 1)
(println '())

;; anything larger will blow the stack
(build-list 1000 add1)

(remove 1 '(1 2 3))
(remove 4 '(1 2 3))
(remove 1 '())
(remove 4 '(1 2 3 4) equal?)
(remove 5 '(1 2 3 4) <)

(list->vector '())
(list->vector '(1 2 3 4))
