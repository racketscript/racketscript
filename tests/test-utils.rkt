#lang racket/base
(require (for-syntax racket/base syntax/parse version/utils)
         syntax/parse/define)
(provide (all-defined-out))

(define-syntax (run-if-version stx)
  (syntax-parse stx
    [(_ v:str test ...)
     #:when (valid-version? (syntax-e #'v))
     (if (version<? (version) (syntax-e #'v))
         #'(void)
         #'(begin test ...))]))
