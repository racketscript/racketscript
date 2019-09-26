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

#;(require (only-in racket/base [lambda err:mz:lambda])) ; so err/rt-test works with beginner.rktl
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
         e)])
  #;(begin
    (do-err/rt-test e . rest)
    (do-err/rt-test (let () e 'not-an-error) . rest)))
#;(define-syntax-rule (err/rt-test/once e . rest)
    (do-err/rt-test e . rest))
#;(define-syntax do-err/rt-test
  (lambda (stx)
    (syntax-case stx ()
      #;[(_ e exn?)
       #'(thunk-error-test (err:mz:lambda () e) (quote-syntax e) exn?)]
      [(_ e exn? msg-rx)
       #'(thunk-error-test
          (err:mz:lambda () e)
          (quote-syntax e)
          (lambda (exn)
            (and (exn? exn)
                 (regexp-match? msg-rx (exn-message exn)))))]
      #;[(_ e)
       #'(do-err/rt-test e exn:application:type?)])))

(define-syntax (run-if-version stx)
  (syntax-parse stx
    [(_ v:str test ...)
     #:when (valid-version? (syntax-e #'v))
     (if (version<? (version) (syntax-e #'v))
         #'(void)
         #'(begin test ...))]))
