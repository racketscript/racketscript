#lang racket/base

(define lam1
  (case-lambda
    [(a b c) (* a b c)]
    [(a b) (+ a b)]))

(displayln (list (lam1 8 2 3) (* 8 2 3)))
(displayln (list (lam1 3 4) (+ 3 4)))
;(check-exn exn:fail? (λ () (lam1 3 4 5 6)) "no matching clause")

(define lam2
  (case-lambda
    [() "duh"]
    [(a b c) (* a b c)]
    [(a b) (+ a b)]
    [v (apply + v)]))

(displayln (list (lam2) "duh"))
(displayln (list (lam2 8 2 3) (* 8 2 3)))
(displayln (list (lam2 3 4) (+ 3 4)))
(displayln (list (lam2 3 4 5 6) (+ 3 4 5 6)))

(define lam3
  (case-lambda
    [() "duh"]
    [(a b c) (* a b c)]
    [(a b) (+ a b)]
    [(a b c . d) (+ a b c (apply * d))]
    [v (apply * v)]))

(displayln (list (lam3) "duh"))
(displayln (list (lam3 8 2 3) (* 8 2 3)))
(displayln (list (lam3 3 4) (+ 3 4)))
(displayln (list (lam3 3 4 5 6) (+ 3 4 5 6)))
(displayln (list (lam3 3 4 5 6 7 8 9) (+ 3 4 5 (* 6 7 8 9))))
