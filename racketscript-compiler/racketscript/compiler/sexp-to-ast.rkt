#lang racket/base

(require racket/match
         racket/pretty
         racket/hash
         "ast.rkt")

(provide parse-linklet)

;; originally written by Sam Tobin-Hochstadt

(define (formals->absyn formals)
  (match formals
    [_ #:when (symbol? formals) formals]
    [_ #:when (list? formals) formals]
    [(list-rest x ... y) (cons x y)]))

(define (formals->bindings formals)
  (define (get-formals formals)
    (match formals
      [_ #:when (symbol? formals) (list formals)]
      [_ #:when (list? formals) formals]
      [(list-rest x ... y) (cons y x)]))

  (for/hash ([i (get-formals formals)])
    (values i 'lexical)))

(define (to-absyn v bindings)
  (define (t v [b bindings]) (to-absyn v b))
  (match v
    [(or (? string?) (? number?) (? boolean?) (? bytes?)) (Quote v)]
    [`(quote ,v) (Quote v)] ;; TODO do we not need to call to-absyn on v?
    [`(begin0 ,e0 ,e1 ...) (Begin0 (t e0) (map t e1))]
    [`(begin ,e ...) (map t e)]
    [`(if ,e0 ,e1 ,e2)
     (If (t e0) (t e1) (t e2))]
    [`(let-values (,[list xs es] ...) ,b)
     (define bindings* (hash-union bindings
                                   (for*/hash ([x xs] [i x]) (values i 'lexical))))
     (LetValues (for/list ([x xs] [e es])
                  (cons x (t e)))
                (list (t b bindings*)))]
    [`(letrec-values (,[list xs es] ...) ,b)
     (define bindings* (hash-union bindings
                                   (for*/hash ([x xs] [i x]) (values i 'lexical))))
     (LetValues (for/list ([x xs] [e es])
                  (cons x (t e bindings*)))
                (list (t b bindings*)))]
    [`(case-lambda . ,clauses)
     (CaseLambda
      (map (λ (c)
             (match c
               ;; TODO in regular to-absyn, the structure is different
               [(list formals body)
                (Lambda (formals->absyn formals)
                        (list (t body (hash-union bindings (formals->bindings formals))))
                        #f)]))
           clauses))]
    [`(lambda ,formals ,body)
     (define fabsyn (formals->absyn formals))
     (Lambda fabsyn (list (t body (hash-union bindings (formals->bindings formals)))) #f)]
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
     ;; FIXME not really always '#%kernel
     (ImportedIdent i '#%kernel #t)]
    [`(set! ,s ,e)
     (Set! s (t e))]
    [`(with-continuation-mark ,key ,value ,result)
     (WithContinuationMark (t key)
                           (t value)
                           (t result))]
    ;; application
    [`(,rator . ,rands)
     (App (t rator) (map t rands))]
    [_ (displayln "unsupported form =>")
       (pretty-print v)
       (error 'linklet-expand)]))


(define (parse-linklet v path)
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
     (Linklet path (cons (list '#%kernel) imports) exports (map (lambda (v) (to-absyn v (hash-union imps defs))) body))]))

