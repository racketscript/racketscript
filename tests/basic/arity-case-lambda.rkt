#lang racket/base

(define lam1
  (case-lambda
    [(a b c) (* a b c)]
    [(a b) (+ a b)]))
(equal? (procedure-arity lam1) '(2 3))

(define lam2
  (case-lambda
    [() "duh"]
    [(a b c) (* a b c)]
    [(a b) (+ a b)]
    [v (apply + v)]))
(equal? (procedure-arity lam2) (make-arity-at-least 0))

(define lam3
  (case-lambda
    [() "duh"]
    [(a b c) (* a b c)]
    [(a b) (+ a b)]
    [(a b c . d) (+ a b c (apply * d))]
    [v (apply * v)]))
(equal? (procedure-arity lam3) (make-arity-at-least 0))

(define lam4
  (case-lambda
    [(a b c) (* a b c)]
    [(a b c d e . f) (+ a b)]))
(equal? (procedure-arity lam4) (list 3 (make-arity-at-least 5)))

(define lam5
  (case-lambda
    [(a b c) (* a b c)]
    [(a b c d e . f) (* a b c)]
    [(a b c d e f g h . i) (+ a b)]))
(equal? (procedure-arity lam5) (list 3 (make-arity-at-least 5)))

(define lam6
  (case-lambda
    [(a b c d e f g h . i) (+ a b)]
    [(a b c) (* a b c)]))
(equal? (procedure-arity lam6) (list 3 (make-arity-at-least 8)))

(define lam7
  (case-lambda
    [(a b c d e f g h . i) (+ a b)]
    [(a b c . d) (* a b c)]))
(equal? (procedure-arity lam7) (make-arity-at-least 3))
