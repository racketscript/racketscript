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
         racket/pretty
         racket/dict racket/match
         racket/format
         racket/extflonum
         racket/syntax
         (for-syntax racket/base))

(require "absyn.rkt"
         "config.rkt"
         "util.rkt")

(provide quick-expand
         open-read-module
         read-module
         convert
         to-absyn)

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
  
  (syntax-parse v
    #:literal-sets ((kernel-literals #:phase (current-phase)))
    [v:str (Quote (syntax-e #'v))]
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
    [(begin0 e0 e ...)
     (map to-absyn (syntax->list #'(e0 e ...)))]
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
    [(#%plain-lambda formals . body)
     (define flist? (list? (syntax-e #'formals)))
     (define fabsyn (if flist?
                        (to-absyn #'formals)
                        (list (syntax-e #'formals))))
     (PlainLambda fabsyn (map to-absyn (syntax->list #'body)) flist?)]
    [(define-values (id ...) b)
     (DefineValues (syntax->datum #'(id ...)) (to-absyn #'b))]
    [(#%top . x) (TopId (syntax-e #'x))]
    [i:identifier #:when (quoted?) (syntax-e #'i)]
    [i:identifier
     (match (identifier-binding #'i)
       ['lexical (syntax-e #'i)]
       [#f (syntax-e #'i)]
       [(list src-mod src-id nom-src-mod mod-src-id src-phase import-phase nominal-export-phase)
        (match-define (list mod-path self?) (index->path nom-src-mod))
        (unless self?
          (module-ident-sources (hash-set (module-ident-sources)
                                          (syntax-e #'i) mod-path)))
        (syntax-e #'i)])]
    [(define-syntaxes (i ...) b) #f]
    [(begin-for-syntax (b ...)) #f]
    [(_ ...) 
     (map to-absyn (syntax->list v))]
    [(a . b)
     (error "impropers are not supported")]
    [#(_ ...) (error "vector not supported")]
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
     (error "with-continuation-mark is not supported")]))

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
