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

(require "absyn.rkt")

(define current-module (make-parameter (list #f)))
(define current-phase (make-parameter 0))
(define quoted? (make-parameter #f))

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
    [((~literal with-continuation-mark) e0 e1 e2)
     (error "with-continuation-mark is not supported")]
    [(begin0 e0 e ...)
     (map to-absyn (syntax->list #'(e0 e ...)))]
    [(if e0 e1 e2)
     (If (to-absyn #'e0) (to-absyn #'e1) (to-absyn #'e2))]
    [(let-values ([xs es] ...) b ...)
     (LetValues (for/list ([x (syntax->list #'(xs ...))]
                           [e (syntax->list #'(es ...))])
                  (cons (to-absyn x) (to-absyn e)))
                (map to-absyn (syntax->list #'(b ...))))]
    [(letrec-values ([xs es] ...) b ...)
     (error "let-rec is not supported")]
    [(quote e) (Quote
                (parameterize ([quoted? #t])
                  (to-absyn #'e)))]

    [(#%require . x) #f] ;; TODO
    [(#%top . x) (TopId (symbol->string (syntax-e #'x)))]
    [(a . b) (error "improper not supported")]
    [i:identifier
     (syntax-e #'i)
     #;(match (identifier-binding #'i)
       ['lexical (hash 'lexical  (symbol->string (syntax-e v)))]
       [#f       (hash 'toplevel (symbol->string (syntax-e v)))]
       [(list (app index->path src) src-id _ _ 0 0 0)
        (hash 'module (symbol->string (syntax-e v))
              'source-module (if (path? src)
                                 (path->string src)
                                 (and src (symbol->string src)))
              'source-name (symbol->string src-id))]
       [v (error 'expand_racket "phase not zero: ~a" v)])]
    [#(_ ...) (error "vector not supported")]
    [_ #:when (box? (syntax-e v))
       (error "box not supported")]
    [_ #:when (exact-integer? (syntax-e v))
       (Quote (~a (syntax-e v)))]
    [_ #:when (boolean? (syntax-e v)) (Quote (syntax-e v))]
    [_ #:when (keyword? (syntax-e v)) (Quote (keyword->string (syntax-e v)))]
    [(~or (~datum +inf.0) (~datum -inf.0) (~datum nan.0))
     (Quote (number->string (syntax-e v)))]
    [_ #:when (real? (syntax-e v)) (Quote (syntax-e v))]
    [_ #:when (char? (syntax-e v))
       (Quote (~a (char->integer (syntax-e v))))]
    [_ #:when (regexp? (syntax-e v))
       (error "regexp not supported")]
    [_ #:when (pregexp? (syntax-e v))
       (error "prefexp? not supported")]
    [_ #:when (byte-regexp? (syntax-e v))
       (error "byte-regexp? not supported")]
    [_ #:when (byte-pregexp? (syntax-e v))
       (error "byte-pregexp not supported")]))

(define (convert mod)
  (syntax-parse mod
    #:literal-sets ((kernel-literals #:phase (current-phase)))
    [(module name:id lang:expr (#%plain-module-begin forms ...))
     (Module (symbol->string (syntax-e #'name))
             #f
             (filter-map to-absyn (syntax->list #'(forms ...))))]
    [_
     (error 'convert "bad ~a ~a" mod (syntax->datum mod))]))

;;; Read modules

(define (read-module input)
  (read-syntax (object-name input) input))

(define (open-read-module in-path)
  (read-module (open-input-file in-path)))

(define (quick-convert in-path)
  (read-accept-reader #t)
  (read-accept-lang #t)
  (convert (do-expand (open-read-module in-path) in-path)))

