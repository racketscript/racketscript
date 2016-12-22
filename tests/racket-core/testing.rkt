#lang racket/base
(require syntax/parse/define)
(provide test err/rt-test)

(define-simple-macro (test expected f . args)
  (let ([res (f . args)]
        [exp expected])
  (displayln (equal? exp res))))

(define-simple-macro (err/rt-test e) e)

