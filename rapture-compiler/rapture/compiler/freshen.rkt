#lang racket

(require (for-syntax syntax/parse)
         syntax/id-table
         syntax/parse
         syntax/stx)

(provide (rename-out [freshen* freshen]))

;; Formals is one of
;; - stx-id
;; - stx-list (xs:id ...)
;; - stx-pair (xs:id ...+ . xn)
 
 (define (freshen* e)
   (freshen e (make-immutable-free-id-table)))

;; formals-dict-set : immutable-dict? stx stx -> immutable-dict?
;; Takes in syntax containing formals and new names for each
;; identifiedr, returning updated identifier `table`
(define (formals-dict-set table keys vals)
  (define-values (keys* vals*)
    (values (if (syntax? keys)
                (syntax-e keys)
                keys)
            (if (syntax? vals)
                (syntax-e vals)
                vals)))
  (match (list keys* vals*)
    [`(,(cons ka kb) ,(cons va vb))
     (dict-set (formals-dict-set table kb vb)
               ka va)]
    [`(,empty ,empty) table]
    [_ #:when (symbol? keys*) (dict-set table keys vals)]))
(module+ test
  (require rackunit)
  
  (define SYMAP1 (formals-dict-set (make-immutable-free-id-table)
                                   #'(a b c)
                                   #'(a1 b1 c1)))

  (define-syntax-rule (check-symap table k v)
    (check-equal? (syntax-e (dict-ref table k #f)) (syntax-e v) ""))
  
  (check-symap SYMAP1 #'a #'a1)
  (check-symap SYMAP1 #'b #'b1)
  (check-symap SYMAP1 #'c #'c1)

  (define SYMAP2 (formals-dict-set SYMAP1 #'(b d) #'(b2 d1)))
  (check-symap SYMAP2 #'b #'b2)
  (check-symap SYMAP2 #'d #'d1)

  (define SYMAP3 (formals-dict-set SYMAP2 #'(x y . a) #'(x1 y1 . a1)))
  (check-symap SYMAP3 #'x #'x1)
  (check-symap SYMAP3 #'y #'y1)
  (check-symap SYMAP3 #'a #'a1)

  (check-equal? (formals-dict-set SYMAP3 #'() #'()) SYMAP3
                "return same table if there is nothing to add")
  
  (check-symap (formals-dict-set SYMAP1 #'g #'g1) #'g #'g1)

  (define SYMAP4 (formals-dict-set SYMAP1 (list #'e #'f #'g) (list #'e2 #'f1 #'g2)))
  (check-symap SYMAP4 #'e #'e2)
  (check-symap SYMAP4 #'f #'f1)
  (check-symap SYMAP4 #'g #'g2))
  
;; bindings-dict-set : immutable-dict? stx-list stx-list -> immutable-dict?
;; Takes a syntax with list of identifiers for let bindings and of freshened
;; names of same structure, and updates the identifier table
(define (bindings-dict-set table keys* vals*)
  (for/fold ([t table])
            ([k (syntax-e keys*)]
             [v (syntax-e vals*)])
    (formals-dict-set t k v)))

;; formals-freshen : stx -> stx
;; Recursively call generate-temporary on each identifier
;; inside syntax object `ids`
(define (formals-freshen ids)
  (syntax-parse ids
    [xs:id (car (generate-temporaries (list ids)))]
    [(xs:id ...)
     #:with fresh-xs (generate-temporaries #'(xs ...))
     #'fresh-xs]
    [(xs:id ...+ . xn:id)
     #:with (fresh-xs ...) (generate-temporaries #'(xs ...))
     #:with fresh-xn (formals-freshen #'xn)
     #`(fresh-xs ... . fresh-xn)]))

;; freshen : stx-expr immutable-free-id-table? -> stx-expr
;; Renames bound ids in e to be unique
(define (freshen e sym-map)
  (syntax-parse e
    #:literal-sets ((kernel-literals))
    [x:id (dict-ref sym-map #'x #'x)]
    [(#%top . x) #`(#%top . #,(freshen #'x sym-map))]
    [(module name forms ...)
     #:with (fresh-forms ...) (stx-map (λ (f) (freshen f sym-map)) #'(forms ...))
     #'(module name fresh-forms ...)]
    [(module* name forms ...)
     #:with (forms-forms ...) (stx-map (λ (f) (freshen f sym-map)) #'(forms ...))
     #'(module* name forms-forms ...)]
    [(#%require x ...) e]
    [((~datum quote-syntax) d) e]
    [((~datum quote) d) e]
    [(#%declare _) e]
    #;[(~or (~datum module)
            (~datum module*)
            (~datum #%require)
            (~datum quote)) e]
    [(#%expression v)
     #`(#%expression #,(freshen #'v sym-map))]
    [(set! s:id e)
     #:with fresh-s (freshen #'s sym-map)
     #:with fresh-e (freshen #'e sym-map)
     #`(set! fresh-s fresh-e)]
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
     #:with fresh-xs (formals-freshen #'xs)
     #:with fresh-body (freshen #'body
                                (formals-dict-set sym-map #'xs #'fresh-xs))
     #'(#%plain-lambda fresh-xs . fresh-body)]
    [(case-lambda . clauses)
     (define (freshen-clause clause)
       (syntax-parse clause
         [(xs . body)
          #:with fresh-xs (formals-freshen #'xs)
          #:with fresh-body (freshen #'body
                                     (formals-dict-set sym-map #'xs #'fresh-xs))
          #'(fresh-xs . fresh-body)]))
     #`(case-lambda #,@(stx-map freshen-clause #'clauses))]
    [(let-values ([xs es] ...) b ...)
     #:with (fresh-xs ...) (stx-map formals-freshen #'(xs ...))
     #:with (fresh-es ...) (stx-map (λ (e)
                                      (freshen e sym-map))
                                    #'(es ...))
     (define sym-map* (bindings-dict-set sym-map
                                         #'(xs ...)
                                         #'(fresh-xs ...)))
     #`(let-values ([fresh-xs fresh-es] ...)
         #,@(stx-map (λ (b) (freshen b sym-map*)) #'(b ...)))]
    [(letrec-values ([xs es] ...) b ...)
     #:with (fresh-xs ...) (stx-map formals-freshen #'(xs ...))
     (define sym-map* (bindings-dict-set sym-map
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
    [(#%plain-app lam arg ...)
     #:with fresh-lam (freshen #'lam sym-map)
     #:with (fresh-arg ...) (stx-map (λ (e) (freshen e sym-map)) #'(arg ...))
     #'(#%plain-app fresh-lam fresh-arg ...)]
    [(#%plain-module-begin form ...)
     #:with (fresh-form ...) (stx-map (λ (f) (freshen f sym-map)) #'(form ...))
     #'(#%plain-module-begin fresh-form ...)]
    [(#%provide p ...) e]
    [(e ...)
     #:with fresh-es (stx-map (λ (e) (freshen e sym-map)) #'(e ...))
     #'fresh-es]
    [_ e]))

(module+ test
  (define-syntax-rule (check-datum-equal? e1 e2)
    (check-equal? (syntax->datum e1) (syntax->datum e1)))

  (define-syntax-rule (run+print e)
    (void (syntax->datum e))
    #;(printf "~a =>\n           ~a\n" 'e (syntax->datum e)))

  (run+print (freshen* #'(#%plain-lambda (a b . c) (foo a b c))))
  (run+print (freshen* #'(#%plain-lambda x (apply + x))))
  (run+print (freshen* #'(begin (#%plain-lambda (x) x)
                                (#%plain-lambda (a b) (+ a b)))))
  (run+print (freshen* #'(let-values ([(a b) (values 1 2)]
                                      [(c d) (values 3 4)])
                           (* a b c d))))
  (run+print (freshen* #'(let-values ([(a b) (values 1 2)]
                                      [(c d) (values 3 4)])
                           (let-values ([(a f) (values a b)]
                                        [(c g) (values c a)])
                             (+ a b c d)))))
  (run+print (freshen* #'(letrec-values ([(a b) (values 1 2)]
                                         [(c d) (values 3 4)])
                           (* a b c d))))
  (run+print (freshen* #'(let-values ([(a b) (values 1 2)]
                                      [(c d) (values 3 4)])
                           (letrec-values ([(a b) (values a b)]
                                           [(c d) (values c a)])
                             (+ a b c d)))))
  (run+print (freshen* #'(let-values ([(a b) (values 1 2)])
                           '(a b))))
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
  
  (run+print (freshen* #'(case-lambda
                           [(x y) (+ x y)]
                           [(x y z) (* x y z)])))

  (run+print (freshen* (expand #'(define (foo a b [c d])
                                   (+ a b))))))
