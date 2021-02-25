#lang racket/base

;; convert a linklet s-exp into abstract syntax
;; linklet grammar described here: https://docs.racket-lang.org/reference/linklets.html

(require racket/match
         racket/pretty
         racket/hash
         "absyn.rkt")
(provide parse-linklet)

(define (formals->absyn formals)
  (match formals
    [(list-rest x ... y) #:when (null? x) y]
    [(list-rest x ... y) #:when (null? y) x]
    [(list-rest x ... y) (list x y)]))

(define (formals->bindings formals)
  (match formals
    [(list-rest x ... y)
     (for/hash ([i (cons y x)]) (values i 'lexical))]))

(define (to-absyn v bindings)
  (define (t v [b bindings]) (to-absyn v b))
  (match v
    [(or (? string?) (? number?) (? boolean?) (? bytes?)) (Quote v)]
    [`(quote ,v) (Quote v)]
    [`(begin0 ,e0 ,e1 ...) (Begin0 (t e0) (map t e1))]
    [`(begin ,e ...) (map t e)]
    [`(if ,e0 ,e1 ,e2)
     (If (t e0) (t e1) (t e2))]
    [`(let-values (,[list xs es] ...) ,b)
     (define bindings* (hash-union bindings
                                   (for*/hash ([x xs] [i x]) (values i 'lexical))))
     (LetValues (for/list ([x xs] [e es])
                  (cons x (t e)))
                (t b))]
    [`(letrec-values (,[list xs es] ...) ,b)
     (define bindings* (hash-union bindings
                                   (for*/hash ([x xs] [i x]) (values i 'lexical))))
     (LetValues (for/list ([x xs] [e es])
                  (cons x (t e bindings*)))
                (t b bindings*))]
    [`(case-lambda . ,clauses)
     (CaseLambda
      (map (λ (c)
             (match c
               [(list formals body)
                (PlainLambda (formals->absyn formals)
                             (list (t body (hash-union bindings (formals->bindings formals))))
                             #f)]))
           clauses))]
    [`(lambda ,formals ,body)
     (define fabsyn (formals->absyn formals))
     (PlainLambda fabsyn (list (t body (hash-union bindings (formals->bindings formals)))) #f)]
    [`(define-values (,name) (#%js-ffi 'require (quote ,mod)))
     ;; HACK: Special case for JSRequire
     (JSRequire name mod 'default)]
    [`(define-values (,name) (#%js-ffi 'require '* (quote ,mod)))
     ;; HACK: Special case for JSRequire
     (JSRequire name mod '*)]
    [`(define-values (,id ...) ,b)
     (DefineValues id (t b))]
    [`(#%variable-reference ,x) (VarRef x)]
    [`(#%variable-reference) (VarRef #f)]
    [(? symbol? i)
     #:when (eq? 'define (hash-ref bindings i #f))
     (TopLevelIdent i)]
    [(? symbol? i)
     #:when (number? (hash-ref bindings i #f))
     (LinkletImportIdent i (hash-ref bindings i))]
    [(? symbol? i)
     #:when (eq? 'lexical (hash-ref bindings i #f))
     (LocalIdent i)]
    [(? symbol? i)
     (PrimitiveIdent i)]
    [`(set! ,s ,e)
     (Set! s (t e))]
    [`(with-continuation-mark ,key ,value ,result)
     (WithContinuationMark (t key)
                           (t value)
                           (t result))]
    ;; application
    [`(,rator . ,rands)
     (PlainApp (t rator) (map t rands))]
    [_ (displayln "unsupported form =>")
       (pretty-print v)
       (error 'linklet-expand)]))




(define (parse-linklet v)
  (match v
    [`(linklet ,imports ,exports ,@body)
     (define imps (for*/hash ([(j import) (in-indexed imports)]
                              [i import])
                    (values i j)))
     (define defs (make-hash))
     (for ([b body])
       (match b
         [`(define-values ,is ,_)
          (for ([i is]) (hash-set! defs i 'define))]
         [_ (void)]))
     (Linklet imports exports (map (lambda (v) (to-absyn v (hash-union imps defs))) body))]))


(module+ main
  (parse-linklet `(linklet () () (lambda (x) (+ x 3))))
  )
