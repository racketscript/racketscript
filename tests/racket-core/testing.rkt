#lang racket/base
(require (for-syntax racket/base syntax/parse version/utils)
         syntax/parse/define)
(provide (all-defined-out))

(define (test expected maybe-fn . args)
  (if (null? args)
      (displayln (equal? expected maybe-fn))
      (displayln (equal? expected
                         (if (procedure? maybe-fn)
                             (apply maybe-fn args)
                             (car args))))))

(define-simple-macro (err/rt-test e . _) e)

(define-syntax (run-if-version stx)
  (syntax-parse stx
    [(_ v:str test ...)
     #:when (valid-version? (syntax-e #'v))
     (if (version<? (version) (syntax-e #'v))
         #'(void)
         #'(begin test ...))]))
