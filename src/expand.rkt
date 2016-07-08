#lang racket/base

;; Ripped out of Pycket project. Orignial source code at -
;;
;;   https://github.com/samth/pycket/blob/master/pycket/pycket-lang/expand.rkt
;;
;; Copyright (c) 2013 Sam Tobin-Hochstadt, Jeremy Siek, Carl Friedrich Bolz

(require syntax/parse syntax/modresolve
         (only-in racket/list append-map last-pair filter-map first add-between)
         racket/path
         racket/bool
         racket/list
         racket/function
         racket/pretty
         racket/dict racket/match
         racket/vector
         racket/format
         racket/extflonum
         racket/syntax
         syntax/stx
         (for-syntax racket/base))

(require "absyn.rkt"
         "config.rkt"
         "case-lambda.rkt"
         "util.rkt")

(provide quick-expand
         open-read-module
         read-module
         convert
         to-absyn
         to-absyn/top)

(define current-module (make-parameter (list #f)))
(define current-phase (make-parameter 0))
(define quoted? (make-parameter #f))

(define module-ident-sources (make-parameter #f))

(define (do-expand stx in-path)
  ;; error checking
  (syntax-parse stx
    [((~and mod-datum (~datum module)) n:id lang:expr . rest)
     (void)]
    [((~and mod-datum (~datum module)) . rest)
     (error 'do-expand "got ill-formed module: ~a\n" (syntax->datum #'rest))]
    [rest
     (error 'do-expand "got something that isn't a module: ~a\n" (syntax->datum #'rest))])
  ;; work
  
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-syntax-introduce (expand stx))))

;;;; Module paths

(define (index->path i)
  (define-values (v u) (module-path-index-split i))
  (if v
      (list (resolved-module-path-name (module-path-index-resolve i)) #f)
      (list (current-module) #t)))

(define (full-path-string p)
  (path->string (normalize-path (simplify-path p #t))))
(define (desymbolize s)
  (cond
    [(symbol? s) (symbol->string s)]
    [(path? s)   (full-path-string s)]
    [else        s]))

(define (make-path-strings xs)
  (define (path-string p)
    (if (path? p)
      (path->string
        (simplify-path p))
      p))
  (map path-string xs))

(define (resolve-module mod-name)
  (if (memv mod-name (list "." ".."))
    (list mod-name)
    (with-handlers
      ([exn:fail:filesystem:missing-module?
         (lambda (e)
           (make-path-strings
             (append (current-module) (list (desymbolize mod-name)))))])
      (list
        (full-path-string
          (resolve-module-path mod-name #f))))))

;;;; conversion and expansion

(define (expanded-module)
  (let ([mod (car (current-module))]
        [path (cdr (current-module))])
    (if (not mod)
      ;; If we don't have the module name, encode it relative to
      ;; the current module
      (if (null? path) '(".") (map (λ (_) "..") (cdr (current-module))))
      (list (full-path-string mod)))))

(define (list-module-path p)
  (if (not (path? (car p)))
    (append (expanded-module) (map desymbolize (cdr p)))
    (map desymbolize p)))

(define (symbol-module-path p)
  (if (string=? (symbol->string p) "expanded module")
    (expanded-module)
    (list (symbol->string p))))

(define (require-parse r)
  (syntax-parse r
    [v:str (Require (syntax-e #'v) #f)]
    [v:identifier (Require (syntax-e #'v) #f)]
    [_ (error "unsupported require format")]))

(define (provide-parse r)
  (syntax-parse r
    [v:str (Provide (syntax-e #'v))]
    [v:identifier (Provide (syntax-e #'v))]
    [_ (error "unsupported provide form")]))

(define (to-absyn v)
  (define (proper l)
    (match l
      [(cons a b) (cons a (proper b))]
      [_ null]))

  (define (formals->absyn formals)
    (let ([f (to-absyn formals)])
      (cond
        [(or (list? f) (symbol? f)) f]
        [(cons? f) (let-values ([(fp fi) (splitf-at f identity)])
                     (cons fp fi))]
        [else (error 'λ "invalid λ formals")])))
  
  (syntax-parse v
    #:literal-sets ((kernel-literals #:phase (current-phase)))
    [v:str (syntax-e #'v)]
    ;; special case when under quote to avoid the "interesting"
    ;; behavior of various forms
    [(_ ...) 
     #:when (quoted?)
     (map to-absyn (syntax->list v))]
    [(module _ ...) #f] ;; ignore these
    [(module* _ ...) #f] ;; ignore these
    ;; this is a simplification of the json output
    [(#%plain-app e0 e ...)
     (PlainApp (to-absyn #'e0) (map to-absyn (syntax->list #'(e ...))))]
    [(#%expression e) (to-absyn #'e)]
    [(begin e ...)
     (map to-absyn (syntax->list #'(e ...)))]
    [(begin0 e0 e ...)
     (Begin0
       (to-absyn #'e0)
       (map to-absyn (syntax->list #'(e ...))))]
    [(if e0 e1 e2)
     (If (to-absyn #'e0) (to-absyn #'e1) (to-absyn #'e2))]
    [(let-values ([xs es] ...) b ...)
     (LetValues (for/list ([x (syntax->list #'(xs ...))]
                           [e (syntax->list #'(es ...))])
                  (cons (syntax->datum x)
                        (to-absyn e)))
                (map to-absyn (syntax->list #'(b ...))))]
    [(letrec-values ([xs es] ...) b ...)
     (LetRecValues (for/list ([x (syntax->list #'(xs ...))]
                              [e (syntax->list #'(es ...))])
                     (cons (syntax->datum x)
                           (to-absyn e)))
                   (map to-absyn (syntax->list #'(b ...))))]
    [(quote e) (Quote
                (parameterize ([quoted? #t])
                  (to-absyn #'e)))] ;;;; TODO: HACK! See what actually happens
    [(#%require x ...)
     #f
     #;(map require-parse (syntax->list #'(x ...)))]
    [(#%provide x ...)
     (map provide-parse (syntax->list #'(x ...)))]
    [(case-lambda . clauses)
     (CaseLambda
      (stx-map (λ (c)
                 (syntax-parse c
                   [(formals . body) (PlainLambda (formals->absyn #'formals)
                                                  (stx-map to-absyn #'body))]))
               #'clauses))]
    [(#%plain-lambda formals . body)
     (define fabsyn (formals->absyn #'formals))
     (PlainLambda fabsyn (map to-absyn (syntax->list #'body)))]
    [(define-values (id ...) b)
     (DefineValues (syntax->datum #'(id ...)) (to-absyn #'b))]
    [(#%top . x) (TopId (syntax-e #'x))]
    [i:identifier #:when (quoted?) (syntax-e #'i)]
    [i:identifier
     (match (identifier-binding #'i)
       ['lexical (syntax-e #'i)]
       [#f (syntax-e #'i)]
       [(list src-mod src-id nom-src-mod mod-src-id src-phase import-phase nominal-export-phase)
        (define (rename n)
          ;; TODO: quick hack for null keyword. Proabably out previous
          ;; table of base symbols was actually useful
          (cond 
            [(equal? n 'null) 'racket_null]
            [else n]))
        (match-define (list mod-path self?) (index->path nom-src-mod))
        (unless self?
          (module-ident-sources (hash-set (module-ident-sources)
                                          (rename (syntax-e #'i)) mod-path)))
        (syntax-e #'i)])]
    [(define-syntaxes (i ...) b) #f]
    [(begin-for-syntax b ...) #f]
    [(_ ...) 
     (map to-absyn (syntax->list v))]
    [(a . b)
     (cons (to-absyn #'a) (to-absyn #'b))]
    [#(_ ...) (vector-map to-absyn (syntax-e v))]
    [_ #:when (number? (syntax-e v)) (syntax-e v)]
    [_ #:when (boolean? (syntax-e v)) (syntax-e v)]
    [_ #:when (prefab-struct-key (syntax-e v)) #f] ;; TODO: No error to compile FFI
    [_ #:when (box? (syntax-e v))
       (error "box not supportend")]
    [_ #:when (exact-integer? (syntax-e v))
       (Quote (syntax-e v))]
    [_ #:when (boolean? (syntax-e v)) (Quote (syntax-e v))]
    [_ #:when (keyword? (syntax-e v)) (Quote (syntax-e v))]
    [(~or (~datum +inf.0) (~datum -inf.0) (~datum nan.0))
     (Quote (syntax-e v))]
    [_ #:when (real? (syntax-e v)) (Quote (syntax-e v))]
    [_ #:when (char? (syntax-e v))
       (Quote (syntax-e v))]
    [_ #:when (regexp? (syntax-e v))
       (error "regexp not supported")]
    [_ #:when (pregexp? (syntax-e v))
       (error "prefexp? not supported")]
    [_ #:when (byte-regexp? (syntax-e v))
       (error "byte-regexp? not supported")]
    [_ #:when (byte-pregexp? (syntax-e v))
       (error "byte-pregexp not supported")]
    [((~literal with-continuation-mark) e0 e1 e2)
     (error "with-continuation-mark is not supported")]
    [_ (displayln "unsupported form =>")
       (pretty-print (syntax->datum v))
       (error 'expand)]))

(define (convert mod path)
  (syntax-parse mod
    #:literal-sets ((kernel-literals #:phase (current-phase)))
    [(module name:id lang:expr (#%plain-module-begin forms ...))
     (parameterize ([module-ident-sources (hash)])
       (define mod-id (syntax-e #'name))
       (printf "[absyn] ~a\n" mod-id)
       (let* ([ast (filter-map to-absyn (syntax->list #'(forms ...)))]
              [mod (module-ident-sources)]
              [imports (assocs->hash-list (map reverse-pair (hash->list mod)))])
         (Module mod-id
                 path
                 (syntax->datum #'lang)
                 imports
                 ast)))]
    [_
     (error 'convert "bad ~a ~a" mod (syntax->datum mod))]))

(define (to-absyn/top stx)
  (parameterize ([module-ident-sources (hash)])
    (to-absyn stx)))

;;; Read modules

(define (read-module input)
  (read-syntax (object-name input) input))

(define (open-read-module in-path)
  (read-module (open-input-file in-path)))

(define (quick-expand in-path)
  (printf "[expand] ~a\n" in-path)
  (read-accept-reader #t)
  (read-accept-lang #t)
  (define full-path (path->complete-path in-path))
  (parameterize ([current-directory (path-only full-path)])
    (do-expand (open-read-module in-path) in-path)))

(module+ test
  (require rackunit)
  (define-syntax-rule (to-absyn/expand stx)
    (to-absyn/top (expand stx)))

;;; Check values

  (check-equal? (to-absyn/expand #'42)
                (Quote 42))
  (check-equal? (to-absyn/expand "Hello World")
                (Quote "Hello World"))
  (check-equal? (to-absyn/expand #`'symbol)
                (Quote 'symbol))
  (check-equal? (to-absyn/expand #'#(1 2 3 4))
                (Quote #(1 2 3 4))
                "vector")
  (check-equal? (to-absyn/expand #''(1 2 3 4))
                (Quote '(1 2 3 4))
                "list")
  (check-equal? (to-absyn/expand #''(1 2 "hi!" 3 #(1 2 3)))
                (Quote '(1 2 "hi!" 3 #(1 2 3))))
  (check-equal? (to-absyn/expand #'#f)
                (Quote #f))


  ;; Check lambdas

  (check-equal? (to-absyn/expand #`(λ (x) x))
                (PlainLambda '(x) (list 'x)))
  (check-equal? (to-absyn/expand #`(λ x x))
                (PlainLambda 'x (list 'x)))
  (check-equal? (to-absyn/expand #`(λ (a b . c) (+ a b (apply + c))))
                (PlainLambda
                 '((a b) . c)
                 (list
                  (PlainApp '+
                            (list 'a 'b
                                  (PlainApp 'apply (list '+ 'c)))))))

  ;; Check application

  (check-equal? (to-absyn/expand #`(displayln "hello"))
                (PlainApp 'displayln (list (Quote "hello"))))
  (check-equal? (to-absyn/expand #`((λ (x) x) 42))
                (PlainApp (PlainLambda '(x) '(x))
                          (list (Quote 42))))

  ;; If expresion

  (check-equal? (to-absyn/expand #`(if #t 'yes 'no))
                (If (Quote #t) (Quote 'yes) (Quote 'no)))

  ;; Check TopId, should happen rarely
  (check-equal? (to-absyn/expand #'x)
                (TopId 'x))

  ;; Check let values, lambdas, applications and more

  (check-equal? (to-absyn/expand #'(let-values ([(a) 1] [(b) 2])
                                     a b))
                (LetValues (list (cons '(a) (Quote 1))
                                 (cons '(b) (Quote 2)))
                           (list 'a 'b))
                "let values")
  (check-equal? (to-absyn/expand #'(let-values ([(a) '(1 2)] [(b) (+ 2 4)])
                                     a b))
                (LetValues (list (cons '(a) (Quote '(1 2)))
                                 (cons '(b) (PlainApp '+ (list (Quote 2) (Quote 4)))))
                           (list 'a 'b)))

  (check-equal? (to-absyn/expand
                 #`(define-values (fact)
                     (λ (n)
                       (if (zero? n)
                           0
                           (* n (fact (sub1 n)))))))
                (DefineValues '(fact)
                  (PlainLambda
                   '(n)
                   (list
                    (If (PlainApp 'zero? '(n))
                        (Quote 0)
                        (PlainApp
                         '*
                         (list 'n
                               (PlainApp
                                (TopId 'fact)
                                (list (PlainApp 'sub1 (list 'n)))))))))))

  (check-equal?
   (to-absyn/expand
    #`(letrec-values
          ([(even? odd?)
            (values
             (lambda (n)
               (or (zero? n)
                   (odd? (sub1 n))))
             (lambda (n)
               (or (not (zero? n))
                   (even? (sub1 n)))))])
        (or (even? 50))))
   (LetRecValues
    (list
     (cons
      '(even? odd?)
      (PlainApp
       'values
       (list
        (PlainLambda
         '(n)
         (list
          (LetValues
           (list (cons '(or-part) (PlainApp 'zero? '(n))))
           (list
            (If
             'or-part
             'or-part
             (PlainApp 'odd? (list (PlainApp 'sub1 '(n)))))))))
        (PlainLambda
         '(n)
         (list
          (LetValues
           (list (cons '(or-part) (PlainApp 'not (list (PlainApp 'zero? '(n))))))
           (list
            (If
             'or-part
             'or-part
             (PlainApp 'even? (list (PlainApp 'sub1 '(n)))))))))))))
    (list (PlainApp 'even? (list (Quote 50))))))
  
;;; Begin expressions

  (check-equal?
   (to-absyn/expand #'(begin
                        (displayln "Hello!")
                        (displayln "Begin")
                        (displayln "Expression")))
   (list
    (PlainApp 'displayln (list (Quote "Hello!")))
    (PlainApp 'displayln (list (Quote "Begin")))
    (PlainApp 'displayln (list (Quote "Expression")))))

  (check-equal?
   (to-absyn/expand #'(begin0 (displayln "Hello!")
                        (displayln "Begin")
                        (displayln "Expression")))
   (Begin0
     (PlainApp 'displayln (list (Quote "Hello!")))
     (list
      (PlainApp 'displayln (list (Quote "Begin")))
      (PlainApp 'displayln (list (Quote "Expression"))))))

  (check-equal?
   (to-absyn/expand #'(define (foobar a b c)
                        (displayln a)
                        (displayln b)
                        (displayln c)))
   (DefineValues
     '(foobar)
     (PlainLambda
      '(a b c)
      (list
       (PlainApp 'displayln (list 'a))
       (PlainApp 'displayln (list 'b))
       (PlainApp 'displayln (list 'c))))))

;;; Case Lambda

  (check-equal?
   (to-absyn/expand (expand #'(case-lambda
                                [(a b) (+ a b)]
                                [(a b c) (* a b c)])))
   (CaseLambda
    (list
     (PlainLambda '(a b) (list (PlainApp '+ '(a b))))
     (PlainLambda '(a b c) (list (PlainApp '* '(a b c)))))))

;;; Check module

  (test-case "simple module"
    (define module-output (convert (expand
                                    #'(module foo racket/base
                                        (provide foo)
                                        (define (foo name)
                                          (displayln "Hello"))))
                                   (string->path "test-expand.rkt")))
    (check-equal? (Module-id module-output) 'foo)
    (check-equal? (Module-path module-output) (string->path "test-expand.rkt"))
    (check-equal? (Module-forms module-output)
                  (list
                   (list (Provide 'foo))
                   (DefineValues
                     '(foo)
                     (PlainLambda '(name)
                                  (list
                                   (PlainApp 'displayln (list (Quote "Hello"))))))))))
