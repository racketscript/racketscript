#lang racket/base
(require (for-syntax racket/base syntax/parse version/utils)
         syntax/parse/define)
(provide (all-defined-out))

(define (test expected f . args)
  (if (null? args)
      (displayln (equal? expected f))
      (displayln (equal? expected (apply f args)))))

(define-simple-macro (err/rt-test e) e)

(define-syntax (run-if-version stx)
  (syntax-parse stx
    [(_ v:str test ...)
     #:when (valid-version? (syntax-e #'v))
     (if (version<? (version) (syntax-e #'v))
         #'(void)
         #'(begin test ...))]))
