#lang racket/base
(require (for-syntax racket/base syntax/parse version/utils)
         syntax/parse/define)
(provide (all-defined-out))

;; this file mostly copied from racket-test/tests/racket/testomg.rktl

(define exn:application:mismatch? exn:fail:contract?)
(define exn:application:type? exn:fail:contract?)
(define exn:application:arity? exn:fail:contract:arity?)

(define test
  (let ()
    (define (test* expect fun args kws kvs)
        (let ([res (if (procedure? fun)
                       (if kws (keyword-apply fun kws kvs args) (apply fun args))
                       (car args))])
          (let ([ok? (equal? expect res)])
            (displayln ok?))))
    (define (test/kw kws kvs expect fun . args) (test* expect fun args kws kvs))
    (define (test    expect fun         . args) (test* expect fun args #f #f))
    (make-keyword-procedure test/kw test)))

(define-syntax (err/rt-test stx)
  (syntax-case stx ()
    [(_ e)
    #'(with-handlers ([(λ (x) #t)
                       (λ (ex)
                         (displayln (exn-message ex)))])
        e)]
    [(_ e e?)
    #'(with-handlers ([e?
                       (λ (ex)
                         (displayln (exn-message ex)))])
        e)]
    [(_ e e? msg-rx)
     #'(with-handlers ([(λ (x) #t)
                        (λ (ex)
                          (displayln
                           (regexp-match msg-rx (exn-message ex))))])
         e)]))

(define-syntax (run-if-version stx)
  (syntax-parse stx
    [(_ v:str test ...)
     #:when (valid-version? (syntax-e #'v))
     (if (version<? (version) (syntax-e #'v))
         #'(void)
         #'(begin test ...))]))
