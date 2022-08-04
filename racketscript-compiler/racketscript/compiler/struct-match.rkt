#lang racket/base

(require (for-syntax racket/base racket/struct-info))

(provide struct-match struct-match-define struct-match-lambda)

(define-for-syntax (pat-pred pat)
  (let* ([stx-hd (car (syntax-e pat))]
         [sym (syntax-e stx-hd)])
    (datum->syntax
      stx-hd
      (string->symbol
        (string-append (symbol->string sym) "?"))
      stx-hd
      stx-hd)))

(define-for-syntax (pat-length pat)
  (length (syntax->datum pat)))

(define-for-syntax (pat-ids pat)
  (cdr (syntax-e pat)))

(define-for-syntax (empty-pat? pat)
  (eq? (syntax->datum pat) '_))

(define-for-syntax (pred-pat? pat)
  (syntax-case pat (?)
    [(? pred val) #t]
    [_ #f]))

(define-for-syntax (pred-from-pat pat val)
  (syntax-case pat (?)
    [(? pred _) #`(pred #,val)]))

(define-for-syntax (id-of-pred-pat pat)
  (syntax-case pat (?)
    [(? _ id) #'id]))

(define-for-syntax (extract-guard body)
  (syntax-case body ()
    [(#:when guard-expr . body)
     #'guard-expr]
    [_ #f]))

(define-for-syntax (remove-guard body)
  (syntax-case body ()
    [(#:when guard-expr . body)
     #'body]
    [_ body]))

(define-for-syntax (validate-struct-patterns! patterns)
  (cond
    [(null? patterns) (void)]
    [else
     (syntax-case (car patterns) (?)
       [(? . _) (validate-struct-patterns! (cdr patterns))]
       [(s . pats)
        (if (identifier? #'s)
          (let ([str-info? (syntax-local-value #'s (λ () #f))])
            (if (not (struct-info? str-info?))
              (raise (format "struct-match: ~a is not a struct" (syntax->datum #'s)))
              (let ([info (extract-struct-info str-info?)])
                ;; TODO the spec for struct-info has additional details about what can appear
                ;;      in the struct-info field-accessors position, so it may cause bugs
                (if (not (eq? (length (cadddr info)) (length (syntax->list #'pats))))
                  (raise (format "struct-match: struct ~a expects ~a fields, got ~a in pattern"
                                 #'s
                                 (length (cadddr info))
                                 (length (syntax->list #'pats))))
                  (validate-struct-patterns! (cdr patterns))))))
          (raise (format "struct-match: unrecognizable head pattern ~a" #'s)))]
       [_ (validate-struct-patterns! (cdr patterns))])]))

(define-syntax (struct-match stx)
  (syntax-case stx ()
    [(_ expr [pattern body0 body ...] ...)
     #`(let ([v expr])
         #,(let ([patterns (syntax->list #'(pattern ...))])
             (let loop ([patterns patterns]
                        [bodys (syntax->list #'((body0 body ...) ...))])
               (validate-struct-patterns! patterns)
               (cond
                 [(null? patterns)
                  #'(error 'match "failed ~e" v)]
                 [(empty-pat? (car patterns)) #`(begin . #,(car bodys))]
                 [(pred-pat? (car patterns))
                  #`(if #,(pred-from-pat (car patterns) #'v)
                      #,(if (empty-pat? (id-of-pred-pat (car patterns)))
                          #`(let () . #,(car bodys))
                          #`(let ([#,(id-of-pred-pat (car patterns)) v])
                              . #,(car bodys)))
                      #,(loop (cdr patterns) (cdr bodys)))]
                 [(identifier? (car patterns))
                  #`(let ([#,(car patterns) v]) . #,(car bodys))]
                 [else
                  (define guard? (extract-guard (car bodys)))
                  #`(let* ([vec-v (struct->vector v)]) ;; TODO maybe don't need vector if using pat-pred crap
                      (define (alt) #,(loop (cdr patterns) (cdr bodys)))
                      (cond
                        [(#,(pat-pred (car patterns)) v)
                         (let-values ([#,(pat-ids (car patterns)) (vector->values vec-v 1)])
                           #,(if guard?
                               #`(if #,guard? (begin . #,(remove-guard (car bodys))) (alt))
                               #`(begin . #,(car bodys))))]
                        [else (alt)]))]))))]))

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
