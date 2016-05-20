#lang racket

(require (for-syntax syntax/parse)
         syntax/id-table
         syntax/parse
         syntax/stx)

(provide (rename-out [freshen* freshen]))
 
 (define (freshen* e)
   (freshen e (make-immutable-free-id-table)))

(define current-phase (make-parameter 0))
(define quoted? (make-parameter #f))

;; formals-dict-set : immutable-bound-id-table? stx stx -> stx
;; Takes in syntax containing formals and new names for each
;; identifier, returning updated identifier `table`
(define (formals-dict-set table keys vals)
  (let loop ([keys* (syntax-e keys)]
             [vals* (syntax-e vals)])
    (match (list keys* vals*)
      [`(,(cons ka kb) ,(cons va vb))
       (if (identifier? ka)
           (dict-set (loop kb vb)
                     ka va)
           (formals-dict-set (loop kb vb)
                             ka va))]
      [`(,null ,null) table]
      [_ #:when (symbol? keys*) (dict-set table keys vals)]
      [`(,a ,b) (dict-set table a b)])))

;; generate-temporaries* : stx -> stx
;; Recursively call generate-temporary on each identifier
;; inside syntax object `ids`
(define (generate-temporaries* ids)
  (cond
    [(identifier? ids) (car (generate-temporaries (list ids)))]
    [(list? ids) (generate-temporaries ids)]
    [(pair? ids) (cons (generate-temporaries* (car ids))
                       (generate-temporaries* (cdr ids)))]
    [(syntax? ids) (let ([ids* (syntax-e ids)])
                     (if (list? ids*)
                         (stx-map generate-temporaries* ids)
                         (generate-temporaries* ids*)))]
    [else (error 'generate-temporaries* "unexpected ~a" ids)]))

;; freshen : stx-expr immutable-free-id-table? -> stx-expr
;; Renames bound ids in e to be unique
(define (freshen e sym-map)
  (syntax-parse e
    #:literal-sets ((kernel-literals))
    [(_ ...) #:when (quoted?) e]
    [x:id #:when (quoted?) (dict-ref sym-map #'x #'x)]
    [x:id (dict-ref sym-map #'x #'x)]
    [v:str #'v]
    [(#%top . x) #`(#%top . #,(freshen #'x sym-map))]
    [(quote e) #'(quote e)]
    [(~or (~literal module)
          (~literal module*)
          (~literal #%require)
          (~literal quote)) e]
    [(#%expression v)
     #`(#%expression #,(freshen #'v sym-map))]
    [(begin0 e0 e ...)
     #`(begin0 #,@(stx-map (λ (e)
                             (freshen e sym-map))
                           #'(e0 e ...)))]
    [(begin e ...)
     #`(begin #,@(stx-map (λ (e)
                            (freshen e sym-map))
                          #'(e ...)))]
    [(if e0 e1 e2)
     #`(if #,(freshen #'e0 sym-map)
           #,(freshen #'e1 sym-map)
           #,(freshen #'e2 sym-map))]
    [(#%plain-lambda xs . body)
     #:with fresh-xs (generate-temporaries* #'xs)
     #:with fresh-body (freshen #'body
                                (formals-dict-set sym-map #'xs #'fresh-xs))
     #'(#%plain-lambda fresh-xs . fresh-body)]
    [(case-lambda (formals body ...+) ...)
     (error 'expand "case-lambda must be expanded already")]
    [(let-values ([xs es] ...) b ...)
     #:with (fresh-xs ...) (generate-temporaries* #'(xs ...))
     #:with (fresh-es ...) (stx-map (λ (e)
                                      (freshen e sym-map))
                                    #'(es ...))
     (define sym-map* (formals-dict-set sym-map
                                        #'(xs ...)
                                        #'(fresh-xs ...)))
     #`(let-values ([fresh-xs fresh-es] ...)
         #,@(stx-map (λ (b) (freshen b sym-map*)) #'(b ...)))]
    [(letrec-values ([xs es] ...) b ...)
     #:with (fresh-xs ...) (generate-temporaries* #'(xs ...))
     (define sym-map* (formals-dict-set sym-map
                                        #'(xs ...)
                                        #'(fresh-xs ...)))
     (with-syntax ([(fresh-es ...) (stx-map (λ (e)
                                              (freshen e sym-map*))
                                            #'(es ...))])
       #`(letrec-values ([fresh-xs fresh-es] ...)
           #,@(stx-map (λ (b) (freshen b sym-map*)) #'(b ...))))]
    [(#%top . x)
     ;; Since we don't rename top levels we don't need to rename these
     e]
    [(define-values (id ...) b)
     ;; TODO: Should we rename toplevels?
     ;; define-values in an internal-defintion context reduces
     ;; to let-values. 
     #`(define-values (id ...) #,(freshen #'b sym-map))]
    [(e ...)
     #:with fresh-es (stx-map (λ (e) (freshen e sym-map)) #'(e ...))
     #'fresh-es]
    [_ e]))

(module+ test
  (define-syntax-rule (check-datum-equal? e1 e2)
    (check-equal? (syntax->datum e1) (syntax->datum e1)))

  (define-syntax-rule (run+print e)
    (printf "~a =>\n           ~a\n" 'e (syntax->datum e)))


  (run+print (freshen* #'(#%plain-lambda (a b . c) (foo a b c))))
  (run+print (freshen* #'(#%plain-lambda x (apply + x))))
  (run+print (freshen* #'(begin (#%plain-lambda (x) x)
                                (#%plain-lambda (a b) (+ a b)))))
  (run+print (freshen* #'(let-values ([(a b) (values 1 2)]
                                      [(c d) (values 3 4)])
                           (* a b c d))))
  (run+print (freshen* (expand #'(begin
                                   (define-values (a b c d) (values 1 2 3 4))
                                   (+ a b c d)))))
  (run+print (freshen* #'(#%plain-lambda (x) x)))
  (run+print (freshen* #'((#%plain-lambda (x y z) (+ x y z)))))
  (run+print (freshen* #'(#%plain-lambda (x)
                                         (#%plain-lambda (x) x))))
  (run+print (freshen* #'((#%plain-lambda (x) x)
                          (#%plain-lambda (x) x))))
  (run+print (freshen* #'(#%plain-lambda (x)
                                         ((#%plain-lambda (x) x)
                                          (#%plain-lambda (x) x)))))
  (run+print (freshen* #'(#%plain-lambda (x)
                                         ((#%plain-lambda (y) x)
                                          (#%plain-lambda (x) x)))))
  )
