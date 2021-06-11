#lang racket/base
(require (for-syntax racket/base syntax/parse))
(provide (all-defined-out))

;; test forms here are duplicated from test-utils.rkt, but
;; for some reason requiring test-utils.rkt doesnt work for racket-core tests

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
     #'(with-handlers ([e?
                        (λ (ex)
                          (displayln
                           (regexp-match msg-rx (exn-message ex))))])
         e)]))

