#lang racket/base

(define x (make-parameter #f))
(define y (make-parameter #f))

(parameterize ([x 1] [y 1])
  (+ (x) (y)))
