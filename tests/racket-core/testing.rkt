#lang racket/base
(require syntax/parse/define)
(provide (all-defined-out))

(define (test expected f . args)
  (if (null? args)
      (displayln (equal? expected f))
      (displayln (equal? expected (apply f args)))))

(define-simple-macro (err/rt-test e) e)
