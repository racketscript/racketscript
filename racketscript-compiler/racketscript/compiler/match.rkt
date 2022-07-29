#lang racket/base

(require (for-syntax racket/base))

;; based on the pattern matcher in schemify

(provide match)

(define-for-syntax (extract-pattern-variables pattern)
  (syntax-case pattern (unquote)
    [(unquote bind-id)
     (if (free-identifier=? #'bind-id #'_)
         null
         (list #'bind-id))]
    [(p1 . p2) (append (extract-pattern-variables #'p1)
                       (extract-pattern-variables #'p2))]
    [else null]))

;; TODO the original implementation uses `wrap-list?` and the other `wrap?` functions,
;; for now I'm not using them, but if it becomes relevant I'll reintroduce them.
;; They seem to be primarily for correlated objects
(define-for-syntax (check-one id pattern head-id)
  (define (check-one/expr e pattern)
    (syntax-case pattern (unquote)
      [(unquote bind-id) #`#t]
      [_ #`(let ([a #,e])
             #,(check-one #'a pattern #f))]))
  (syntax-case pattern (unquote)
    [(unquote bind-id) #`#t]
    [(pat ellipses)
     (and (identifier? #'ellipses)
          (free-identifier=? #'ellipses (quote-syntax ...)))
     (if (syntax-case #'pat (unquote)
           [(unquote bind-id) #t]
           [_ #f])
         #`(list? #,id)
         #`(and (list? #,id)
                (for/and ([v (in-list #,id)])
                  #,(check-one #'v #'pat #f))))]
    [(m-id . p2)
     (and head-id (identifier? #'m-id))
     #`(and (eq? 'm-id #,head-id)
            #,(check-one/expr #`(cdr #,id) #'p2))]
    [(p1 . p2)
     #`(let ([p #,id])
         (and (pair? p)
              #,(check-one/expr #'(car p) #'p1)
              #,(check-one/expr #'(cdr p) #'p2)))]
    [_
     (if (or (identifier? pattern)
             (let ([v (syntax-e pattern)])
               (or (keyword? v)
                   (boolean? v)
                   (null? v))))
         #`(eq? (quote #,pattern) #,id)
         #`(equal? (quote #,pattern) #,id))]))

(define-for-syntax (extract-one id pattern)
  (syntax-case pattern (unquote)
    [(unquote bind-id)
     (if (free-identifier=? #'bind-id #'_)
         #'(values)
         id)]
    [(pat ellipses)
     (and (identifier? #'ellipses)
          (free-identifier=? #'ellipses (quote-syntax ...)))
     (syntax-case #'pat (unquote)
       [(unquote bind-id)
        (if (free-identifier=? #'bind-id #'_)
            #'(values)
            #`(list #,id))]
       [_
        (with-syntax ([pat-ids (extract-pattern-variables #'pat)])
          #`(for/lists pat-ids ([v (in-list #,id)])
              #,(extract-one #'v #'pat)))])]
    [(p1 . p2)
     (let ([ids1 (extract-pattern-variables #'p1)]
           [ids2 (extract-pattern-variables #'p2)])
       (cond
         [(and (null? ids1) (null? ids2))
          #'(values)]
         [(null? ids1)
          #`(let ([d (cdr #,id)])
              #,(extract-one #'d #'p2))]
         [(null? ids2)
          #`(let ([a (car #,id)])
              #,(extract-one #'a #'p1))]
         [else
          #`(let ([p #,id])
              (let-values ([#,ids1 (let ([a (car p)])
                                     #,(extract-one #'a #'p1))]
                           [#,ids2 (let ([d (cdr p)])
                                     #,(extract-one #'d #'p2))])
                (values #,@ids1 #,@ids2)))]))]
    [_
     #'(values)]))

(define-syntax (match stx)
  (syntax-case stx (quasiquote)
    [(_ expr [`pattern body0 body ...] ...)
     #`(let ([v expr])
         #,(let ([patterns (syntax->list #'(pattern ...))])
             (define (build-matches head-id)
               (let loop ([patterns patterns]
                          [bodys (syntax->list #'((body0 body ...) ...))])
                 (cond
                   [(null? patterns)
                    #'(error 'match "failed ~e" v)]
                   [else
                    (define ids (extract-pattern-variables (car patterns)))
                    (define match? (check-one #'v (car patterns) head-id))
                    #`(if #,match?
                          (let-values ([#,ids #,(extract-one #'v (car patterns))])
                            . #,(car bodys))
                          #,(loop (cdr patterns) (cdr bodys)))])))

             ;; If the first pattern is `(<id> ....)`, then
             ;; extract the input head symbol, because we're
             ;; likely to want to check it for many pattern cases
             (syntax-case (and (pair? patterns) (car patterns)) ()
               [(id . _)
                (identifier? #'id)
                #`(let ([hd (and (pair? v) (car v))])
                    #,(build-matches #'hd))]
               [_ (build-matches #f)])))]))
