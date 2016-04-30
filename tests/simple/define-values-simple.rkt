#lang racket/base


(define-values (a b)
  (let-values (((c d) (values 12 24)))
    (values (+ c d) (* c d))))

(displayln a)
(displayln b)
