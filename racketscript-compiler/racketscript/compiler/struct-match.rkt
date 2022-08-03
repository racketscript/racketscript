#lang racket/base

(require (for-syntax racket/base))

(provide struct-match struct-match-define struct-match-lambda)

(define-for-syntax (pat-pred pat)
  (let ([sym (car (syntax->datum pat))])
    (string->symbol
      (string-append (symbol->string sym) "?"))))

(define-for-syntax (pat-length pat)
  (length (syntax->datum pat)))

(define-for-syntax (pat-ids pat)
  (cdr (syntax-e pat)))

(define-for-syntax (empty-pat? pat)
  (eq? (syntax->datum pat) '_))

(define-syntax (struct-match stx)
  (syntax-case stx ()
    [(_ expr [pattern body0 body ...] ...)
     #`(let ([v expr])
         (if (struct? v)
           #,(let ([patterns (syntax->list #'(pattern ...))])
               (let loop ([patterns patterns]
                          [bodys (syntax->list #'((body0 body ...) ...))])
                 (displayln (car patterns))
                 (displayln (car bodys))
                 (cond
                   [(null? patterns)
                    #'(error 'match "failed ~e" v)]
                   [(empty-pat? (car patterns)) #`(begin . #,(car bodys))]
                   [(identifier? (car patterns))
                    #`(let ([#,(car patterns) v])
                        . #,(car bodys))]
                   [else
                    #`(let* ([vec-v (struct->vector v)]) ;; TODO maybe don't need vector if using pat-pred crap
                        (cond
                          [(and (#,(pat-pred (car patterns)) v)
                                (not (= (vector-length vec-v) #,(pat-length (car patterns)))))
                           ;; TODO make better, want to blow up if pattern doesn't correspond to real struct
                           (error 'match "head pattern wrong size")]
                          [(and (#,(pat-pred (car patterns)) v)
                                (= (vector-length vec-v) #,(pat-length (car patterns))))
                           (let-values ([#,(pat-ids (car patterns)) (vector->values vec-v 1)])
                             . #,(car bodys))]
                          [else
                           #,(loop (cdr patterns) (cdr bodys))]))])))
           (error 'match "~e not a struct" v)))]))

(define-syntax (struct-match-define stx)
  (syntax-case stx ()
    [(_ pat expr)
     #`(define-values #,(pat-ids #'pat)
         (let ([v expr])
           (struct-match v
             [pat (values . #,(pat-ids #'pat))])))]))

(define-syntax (struct-match-lambda stx)
  (syntax-case stx ()
    [(_ clause ...) #'(λ (a) (struct-match a clause ...))]))
