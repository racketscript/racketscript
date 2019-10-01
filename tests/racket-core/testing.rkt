#lang racket/base
(require (for-syntax racket/base syntax/parse version/utils)
         syntax/parse/define)
(provide (all-defined-out))

(define exn:application:mismatch? exn:fail:contract?)
(define exn:application:type? exn:fail:contract?)
(define exn:application:arity? exn:fail:contract:arity?)

(define test
  (let ()
    (define (test* expect fun args kws kvs)
      ;; (define form
      ;;   `(,fun ,@args ,@(apply append (if kws (map list kws kvs) '()))))
      ;; (set! number-of-tests (add1 number-of-tests))
      ;; (printf "~s ==> " form)
      ;; (flush-output)
      ;; (with-handlers ([(λ (e) (not (exn:break? e))) ;; handle "exceptions" that are arbitrary values
      ;;                  (λ (e)
      ;;                    (printf "GOT EXN ~s\n" e)
      ;;                    (record-error (list `(EXN ,e) expect form))
      ;;                    (printf "  BUT EXPECTED ~s\n" expect))])
        (let ([res (if (procedure? fun)
                       (if kws (keyword-apply fun kws kvs args) (apply fun args))
                       (car args))])
          ;; (printf "~s\n" res)
          (let ([ok? (equal? expect res)])
            ;; (unless ok?
            ;;   (record-error (list res expect form))
            ;;   (printf "  BUT EXPECTED ~s\n" expect))
            (displayln ok?))))
    (define (test/kw kws kvs expect fun . args) (test* expect fun args kws kvs))
    (define (test    expect fun         . args) (test* expect fun args #f #f))
    (make-keyword-procedure test/kw test)))
#;(define-syntax (test stx)
  (syntax-parse stx
    [(_ expected e) #'(displayln (equal? expected e))]
    [(_ expected maybe-fn . args)
     #'(displayln (equal? expected
                          (if (procedure? maybe-fn)
                              (maybe-fn . args)
                              (car (list . args)))))]))

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
