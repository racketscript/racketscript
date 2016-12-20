#lang racket

(require (for-syntax syntax/parse)
         syntax/id-table
         syntax/parse
         syntax/stx
         racket/syntax
         "stx-utils.rkt")

(provide (rename-out [freshen* freshen]))

;; Formals is one of
;; - stx-id
;; - stx-list (xs:id ...)
;; - stx-pair (xs:id ...+ . xn)
 
 (define (freshen* e)
   (freshen e (make-immutable-free-id-table)))

;; formals-dict-set : immutable-dict? Formals Formals -> immutable-dict?
;; Updates `table` by mapping ids in `keys` formals to new ids `vals`.
(define (formals-dict-set table keys vals)
  (syntax-parse (list keys vals)
    [(x:id y:id) (dict-set table #'x #'y)]
    [((_:id ...) (_:id ...)) (stx-foldl* dict-set table keys vals)]
    [((x:id ...+ . xn:id) (y:id ...+ . yn:id))
     (dict-set (stx-foldl* dict-set table #'(x ...) #'(y ...)) #'xn #'yn)]))
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
  
;; formals-freshen : Formals -> Formals
;; Replace ids in case-lambda/#%plain-lambda formals list with unique ids
(define (formals-freshen ids)
  (syntax-parse ids
    [x:id (generate-temporary #'x)]
    [(x:id ...)
     #:with fresh-xs (generate-temporaries #'(x ...))
     #'fresh-xs]
    [(x:id ...+ . xn:id)
     #:with (fresh-x ... fresh-xn) (generate-temporaries #'(x ... xn))
     #`(fresh-x ... . fresh-xn)]))

;; freshens : stx-forms immutable-free-id-table? -> stx-forms
;; Renames bound ids in fs to be unique
(define (freshens fs sym-map)
  (stx-map (λ (f) (freshen f sym-map)) fs))

;; freshen : stx-form immutable-free-id-table? -> stx-form
;; Renames bound ids in e to be unique
(define (freshen e sym-map)
  (syntax-parse e
    #:literal-sets ((kernel-literals))
    [x:id (dict-ref sym-map #'x #'x)]
    [(#%top . x) #`(#%top . #,(freshen #'x sym-map))]
    [(module name . forms) #`(module name #,@(freshens #'forms sym-map))]
    [(module* name . forms) #`(module* name #,@(freshens #'forms sym-map))]
    [((~or #%require #%provide) x ...) e]
    [((~or (~datum quote-syntax) (~datum quote)) d) e]
    [((~and f (~or begin0 begin
                   #%plain-app
                   #%plain-module-begin
                   with-continuation-mark)) . es)
     #`(f #,@(freshens #'es sym-map))]
    [(#%declare _) e]
    [(#%expression v)
     #`(#%expression #,(freshen #'v sym-map))]
    [(set! s:id e)
     #:with fresh-s (freshen #'s sym-map)
     #:with fresh-e (freshen #'e sym-map)
     #`(set! fresh-s fresh-e)]
    [(if e0 e1 e2)
     #`(if #,(freshen #'e0 sym-map)
           #,(freshen #'e1 sym-map)
           #,(freshen #'e2 sym-map))]
    [(#%plain-lambda xs . body)
     #:with fresh-xs (formals-freshen #'xs)
     #:with fresh-body (freshens #'body
                                 (formals-dict-set sym-map #'xs #'fresh-xs))
     #'(#%plain-lambda fresh-xs . fresh-body)]
    [(case-lambda . clauses)
     (define (freshen-clause clause)
       (syntax-parse clause
         [(xs . body)
          #:with fresh-xs (formals-freshen #'xs)
          #:with fresh-body (freshens #'body
                                      (formals-dict-set sym-map #'xs #'fresh-xs))
          #'(fresh-xs . fresh-body)]))
     #`(case-lambda #,@(stx-map freshen-clause #'clauses))]
    [(let-values ([xs e] ...) b ...)
     #:with (fresh-xs ...) (stx-map generate-temporaries #'(xs ...))
     #:with (fresh-e ...) (freshens #'(e ...) sym-map)
     (define sym-map* (stx-foldl* formals-dict-set sym-map
                                                   #'(xs ...)
                                                   #'(fresh-xs ...)))
     #`(let-values ([fresh-xs fresh-e] ...)
         #,@(freshens #'(b ...) sym-map*))]
    [(letrec-values ([xs e] ...) b ...)
     #:with (fresh-xs ...) (stx-map generate-temporaries #'(xs ...))
     #:do [(define sym-map* (stx-foldl* formals-dict-set sym-map
                                                         #'(xs ...)
                                                         #'(fresh-xs ...)))]
     #:with (fresh-e ...) (freshens #'(e ...) sym-map*)
     #`(letrec-values ([fresh-xs fresh-e] ...)
         #,@(freshens #'(b ...) sym-map*))]
    [(#%top . x)
     ;; Since we don't rename top levels we don't need to rename these
     e]
    [(define-values (id ...) b)
     ;; TODO: Should we rename toplevels?
     ;; define-values in an internal-defintion context reduces
     ;; to let-values. 
     #`(define-values (id ...) #,(freshen #'b sym-map))]
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
