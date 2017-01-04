#lang racket/base

;; Ripped out of Pycket project. Orignial source code at -
;;
;;   https://github.com/samth/pycket/blob/master/pycket/pycket-lang/expand.rkt
;;
;; Copyright (c) 2013 Sam Tobin-Hochstadt, Jeremy Siek, Carl Friedrich Bolz

(require racket/bool
         racket/dict racket/match
         racket/extflonum
         racket/format
         racket/function
         racket/list
         racket/path
         racket/pretty
         racket/set
         racket/syntax
         racket/vector
         syntax/modresolve
         syntax/stx
         syntax/parse
         (only-in racket/list
                  append-map
                  last-pair
                  filter-map
                  first
                  add-between)

         (for-syntax racket/base)

         "absyn.rkt"
         "case-lambda.rkt"
         "config.rkt"
         "global.rkt"
         "logging.rkt"
         "moddeps.rkt"
         "util.rkt")

(provide convert
         open-read-module
         read-module
         to-absyn
         to-absyn/top
         used-idents
         register-ident-use!
         read-and-expand-module
         quick-expand)

(define current-module (make-parameter #f))
(define current-phase (make-parameter 0))
(define quoted? (make-parameter #f))

;;;----------------------------------------------------------------------------
;;;; Module dependencies and imports

;; (Setof (U ModulePath Symbol))
(define current-module-imports (make-parameter (set)))

;; A list of idents used so far while compiling current project
(define used-idents (make-parameter (hash)))

;; Updates `used-ident` by adding identifier `id` imported from `mod-path`.
(define (register-ident-use! mod-path id)
  (used-idents (hash-update (used-idents)
                            mod-path
                            (λ (i*) (set-add i* (list id (current-module))))
                            (set (list id (current-module))))))

;;;----------------------------------------------------------------------------
;;;; Module paths

(define (index->path i)
  (define-values (v u) (module-path-index-split i))
  (if v
      (list (resolved-module-path-name (module-path-index-resolve i)) #f)
      (list (current-module) #t)))

;;;-----------------------------------------------------------------------------
;;;; Conversion and expansion

#;(define (require-parse r)
  (syntax-parse r
    [v:str (Require (syntax-e #'v) #f)]
    [v:identifier (Require (syntax-e #'v) #f)]
    [_ (error "unsupported require format")]))

(define (parse-provide r)
  (syntax-parse r
    [v:identifier (list (SimpleProvide (syntax-e #'v)))]
    [((~datum for-meta) 0 p ...)
     (stx-map (λ (pv) (SimpleProvide (syntax-e pv))) #'(p ...))]
    [((~datum all-defined)) (list (AllDefined (set)))]
    [((~datum all-defined-except) id ...)
     (list (AllDefined (list->set
                        (stx-map syntax-e #'(id ...)))))]
    [((~datum rename) local-id exported-id)
     (list (RenamedProvide (syntax-e #'local-id)
                           (syntax-e #'exported-id)))]
    [((~datum prefix-all-defined) prefix-id)
     (list (PrefixAllDefined (syntax-e #'prefix-id) (set)))]
    [((~datum prefix-all-defined-except) prefix-id id ...)
     (list (PrefixAllDefined (syntax-e #'prefix-id)
                             (list->set
                              (stx-map syntax-e #'(id ...)))))]
    [((~datum all-from) p ...) '()]
    [((~datum all-from-except) p ...) '()]
    [((~datum for-meta) 1 p ...) '()]
    [((~datum for-syntax) p ...) '()]
    [((~datum protect) p ...) '()]
    [_ #;(error "unsupported provide form " (syntax->datum r)) '()]))

(define (to-absyn v)
  (define (formals->absyn formals)
    (parameterize ([quoted? #t])
      (let ([f (to-absyn formals)])
        (cond
          [(or (list? f) (symbol? f)) f]
          [(cons? f) (let-values ([(fp fi) (splitf-at f identity)])
                       (cons fp fi))]
          [else (error 'λ "invalid λ formals")]))))

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
     (LetValues (for/list ([x (syntax->list #'(xs ...))]
                           [e (syntax->list #'(es ...))])
                  (cons (syntax->datum x)
                        (to-absyn e)))
                (map to-absyn (syntax->list #'(b ...))))]
    [(quote e) (Quote
                (parameterize ([quoted? #t])
                  (to-absyn #'e)))] ;;;; TODO: HACK! See what actually happens
    [(quote-syntax e ...) '()]
    [(#%require x ...)
     #f
     #;(map require-parse (syntax->list #'(x ...)))]
    [(#%provide x ...)
     (append-map parse-provide (syntax->list #'(x ...)))]
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
    [(define-values (name) (#%plain-app (~datum #%js-ffi) (~datum 'require) 'mod))
     ;; HACK: Special case for JSRequire
     (JSRequire (syntax-e #'name) (syntax-e #'mod))]
    [(define-values (id ...) b)
     (DefineValues (syntax->datum #'(id ...)) (to-absyn #'b))]
    [(#%top . x) (TopId (syntax-e #'x))]
    [(#%variable-reference x) (to-absyn #'x)]
    [i:identifier #:when (quoted?) (syntax-e #'i)]
    [i:identifier
     (define (rename-module mpath)
       ;; Rename few modules for simpler compilation
       (cond
         [(symbol? mpath) (list #f mpath)]
         [(collects-module? mpath) (list #t '#%kernel)]
         [else (list #f mpath)]))
     (define ident-sym (syntax-e #'i))

     (match (identifier-binding #'i)
       ['lexical (LocalIdent ident-sym)]
       [#f (TopLevelIdent ident-sym)]
       [(list src-mod src-id nom-src-mod mod-src-id src-phase import-phase nominal-export-phase)
        ;; from where we import
        (match-define (list src-mod-path-orig self?) (index->path src-mod))
        (match-define (list nom-src-mod-path-orig _) (index->path nom-src-mod))
        (match-define (list module-renamed? src-mod-path) (rename-module src-mod-path-orig))

        (cond
          [self? (LocalIdent ident-sym)]
          [else
           ;; Add the module from where we actual import this, so that we import this, and
           ;; any side-effects due to this module is actually executed
           ;(match-define (list nom-mod-path _) (index->path nom-src-mod))
           ;(current-module-imports (set-add (current-module-imports) nom-mod-path))

           ;; And still add the actual module where identifier is defined for easy
           ;; and compact import. NOTE:In future we may want to remove this and
           ;; compute this with moddeps information.
           (current-module-imports (set-add (current-module-imports) src-mod-path))

           ;; If we can't follow this symbol, we probably got this
           ;; because of some macro expansion. TODO: As we process
           ;; modules in topological order, we can save this
           ;; identifier, so that when we export this identifier from
           ;; its source module processed later.
           (define path-to-symbol (follow-symbol (global-export-graph)
                                                 src-mod-path-orig
                                                 mod-src-id))
           (unless path-to-symbol
             (hash-update! global-unreachable-idents
                           src-mod-path
                           (λ (s*)
                             (set-add s* mod-src-id))
                           (set mod-src-id)))

           ;; If the moduele is renamed use the id name used at the importing
           ;; module rather than defining module. Since renamed, module currently
           ;; are #%kernel which we write ourselves in JS we prefer original name.
           ;; TODO: We potentially might have clashes, but its unlikely.
           (define-values (effective-id effective-mod)
             (cond
               [module-renamed? (values mod-src-id src-mod-path)]
               [(false? path-to-symbol)
                (values mod-src-id src-mod-path)]
               [else
                (match-let ([(cons (app last mod) (? symbol? id)) path-to-symbol])
                  (values id mod))]))

           (register-ident-use! effective-mod effective-id)
           (ImportedIdent effective-id effective-mod)])])]
    [(define-syntaxes (i ...) b) #f]
    [(set! s e)
     (Set! (syntax-e #'s) (to-absyn #'e))]
    [(with-continuation-mark key value result)
     (WithContinuationMark (to-absyn #'key)
                           (to-absyn #'value)
                           (to-absyn #'result))]
    [(begin-for-syntax b ...) #f]
    [(_ ...)
     (map to-absyn (syntax->list v))]
    [(a . b)
     (cons (to-absyn #'a) (to-absyn #'b))]
    [#(_ ...) (vector-map to-absyn (syntax-e v))]
    [_ #:when (hash? (syntax-e v))
       (define val (syntax-e v))
       (define keys (to-absyn (datum->syntax #'lex (hash-keys val))))
       (define vals (to-absyn (datum->syntax #'lex (hash-values val))))
       (define hash-maker
         (cond
           [(hash-eq? val) make-immutable-hasheq]
           [(hash-eqv? val) make-immutable-hasheqv]
           [(hash-equal? val) make-immutable-hash]))
       (hash-maker (map cons keys vals))]
    [_ #:when (number? (syntax-e v)) (syntax-e v)]
    [_ #:when (bytes? (syntax-e v)) (syntax-e v)]
    [_ #:when (boolean? (syntax-e v)) (syntax-e v)]
    [_ #:when (prefab-struct-key (syntax-e v)) #f] ;; TODO: No error to compile FFI
    [_ #:when (box? (syntax-e v)) (box (parameterize ([quoted? #t])
                                         (to-absyn (unbox (syntax-e v)))))]
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
       (Quote (syntax-e v))]
    [_ #:when (pregexp? (syntax-e v))
       (Quote (syntax-e v))]
    [_ #:when (byte-regexp? (syntax-e v))
       (Quote (syntax-e v))]
    [_ #:when (byte-pregexp? (syntax-e v))
       (Quote (syntax-e v))]
    [_ #:when (void? (syntax-e v))
       (Quote (void))]
    [_ (displayln "unsupported form =>")
       (pretty-print (syntax->datum v))
       (error 'expand)]))

(define (convert mod path)
  (syntax-parse mod
    #:literal-sets ((kernel-literals #:phase (current-phase)))
    [(module name:id lang:expr (#%plain-module-begin forms ...))
     (parameterize ([current-module path]
                    [current-module-imports (set)]
                    [current-directory (path-only path)])
       (define mod-id (syntax-e #'name))
       (log-rjs-info "[absyn] ~a" mod-id)
       (let* ([ast (filter-map to-absyn (syntax->list #'(forms ...)))]
              [imports (current-module-imports)])
         (Module mod-id
                 path
                 (syntax->datum #'lang)
                 imports
                 ast)))]
    [_
     (error 'convert "bad ~a ~a" mod (syntax->datum mod))]))

(define (to-absyn/top stx)
  (to-absyn stx))

(define (do-expand stx in-path)
  ;; error checking
  (syntax-parse stx
    [((~and mod-datum (~datum module)) n:id lang:expr . rest)
     (void)]
    [((~and mod-datum (~datum module)) . rest)
     (error 'do-expand
            "got ill-formed module: ~a\n" (syntax->datum #'rest))]
    [rest
     (error 'do-expand
            "got something that isn't a module: ~a\n" (syntax->datum #'rest))])
  ;; work

  (parameterize ([current-namespace (make-base-namespace)])
    (expand stx)))

;;; Read modules

(define (read-module input)
  (read-syntax (object-name input) input))

(define (open-read-module in-path)
  (call-with-input-file in-path
    (λ (in)
      (read-module in))))

(define (quick-expand in-path)
  (log-rjs-info "[expand] ~a" in-path)
  (read-accept-reader #t)
  (read-accept-lang #t)
  (define full-path (path->complete-path in-path))
  (parameterize ([current-directory (path-only full-path)])
    (do-expand (open-read-module in-path) in-path)))

(define (read-and-expand-module input)
  (read-accept-reader #t)
  (read-accept-lang #t)
  ;; Just give it any name for now
  (define full-path
    (match (object-name input)
      ['stdin (main-source-file)]
      [v (path->complete-path v)]))
  (define new-cwd (if full-path
                      (path-only full-path)
                      (current-directory)))
  (parameterize ([current-directory new-cwd])
    (do-expand (read-syntax (object-name input) input) full-path)))

;;;----------------------------------------------------------------------------

(module+ test
  (require rackunit)
  (define-syntax-rule (to-absyn/expand stx)
    (parameterize ([global-export-graph (hash)])
      (to-absyn/top (expand stx))))
  (define (ident i)
    (match (identifier-binding i)
      ['lexical (LocalIdent (syntax-e i))]
      [#f (TopLevelIdent (syntax-e i))]
      [(list mod-path mod-id _ _ _ _ _)
       (match-define (list mod-path* _) (index->path mod-path))
       (ImportedIdent mod-id mod-path*)]))

;;; Check values

  (check-equal? (to-absyn/expand #'42)
                (Quote 42))
  (check-equal? (to-absyn/expand #'"Hello World")
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

  ;; Check imported ident

  ;;TODO: We rename library modules, so ignore this test for now
  #;(check-equal? (to-absyn/expand #'displayln)
                (ident #'displayln))

  ;; Check lambdas

  (check-equal? (to-absyn/expand #`(λ (x) x))
                (PlainLambda '(x) (list (LocalIdent 'x))))
  (check-equal? (to-absyn/expand #`(λ x x))
                (PlainLambda 'x (list (LocalIdent 'x))))
  (check-equal? (to-absyn/expand #`(λ (a b . c) (+ a b (reduce + c))))
                (PlainLambda
                 '((a b) . c)
                 (list
                  (PlainApp (ident #'+)
                            (list (LocalIdent 'a) (LocalIdent'b)
                                  (PlainApp (TopId'reduce)
                                            (list (ident #'+) (LocalIdent 'c))))))))
  ;; Check application

  (check-equal? (to-absyn/expand #`(write "hello"))
                (PlainApp (ident #'write) (list (Quote "hello"))))
  (check-equal? (to-absyn/expand #`((λ (x) x) 42))
                (PlainApp (PlainLambda '(x) (list (LocalIdent 'x)))
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
                           (list (LocalIdent 'a) (LocalIdent 'b)))
                "let values")

  (check-equal? (to-absyn/expand #'(let-values ([(a) '(1 2)] [(b) (+ 2 4)])
                                     a b))
                (LetValues (list (cons '(a) (Quote '(1 2)))
                                 (cons '(b) (PlainApp (ident #'+) (list (Quote 2) (Quote 4)))))
                           (list (LocalIdent 'a) (LocalIdent 'b))))

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
                    (If (PlainApp (ident #'zero?) (list (LocalIdent 'n)))
                        (Quote 0)
                        (PlainApp
                         (ident #'*)
                         (list (LocalIdent 'n)
                               (PlainApp
                                (TopId 'fact)
                                (list (PlainApp (ident #'sub1)
                                                (list (LocalIdent 'n))))))))))))
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
   (LetValues
    (list
     (cons
      '(even? odd?)
      (PlainApp
       (ident #'values)
       (list
        (PlainLambda
         '(n)
         (list
          (LetValues
           (list (cons '(or-part) (PlainApp (ident #'zero?) (list (LocalIdent 'n)))))
           (list
            (If
             (LocalIdent 'or-part)
             (LocalIdent 'or-part)
             (PlainApp (LocalIdent 'odd?)
                       (list (PlainApp (ident #'sub1) (list (LocalIdent 'n))))))))))
        (PlainLambda
         '(n)
         (list
          (LetValues
           (list (cons '(or-part) (PlainApp (ident #'not)
                                            (list (PlainApp (ident #'zero?)
                                                            (list (LocalIdent 'n)))))))
           (list
            (If
             (LocalIdent 'or-part)
             (LocalIdent 'or-part)
             (PlainApp (LocalIdent 'even?)
                       (list (PlainApp (ident #'sub1) (list (LocalIdent 'n))))))))))))))
    (list (PlainApp (LocalIdent 'even?) (list (Quote 50))))))

;;; Begin expressions

  (check-equal?
   (to-absyn/expand #'(begin
                        (write "Hello!")
                        (write "Begin")
                        (write "Expression")))
   (list
    (PlainApp (ident #'write) (list (Quote "Hello!")))
    (PlainApp (ident #'write) (list (Quote "Begin")))
    (PlainApp (ident #'write) (list (Quote "Expression")))))

  (check-equal?
   (to-absyn/expand #'(begin0 (write "Hello!")
                        (write "Begin")
                        (write "Expression")))
   (Begin0
     (PlainApp (ident #'write) (list (Quote "Hello!")))
               (list
                (PlainApp (ident #'write) (list (Quote "Begin")))
                (PlainApp (ident #'write) (list (Quote "Expression"))))))

  (check-equal?
   (to-absyn/expand #'(define (foobar a b c)
                        (write a)
                        (write b)
                        (write c)))
   (DefineValues
     '(foobar)
     (PlainLambda
      '(a b c)
      (list
       (PlainApp (ident #'write) (list (LocalIdent'a)))
       (PlainApp (ident #'write) (list (LocalIdent'b)))
       (PlainApp (ident #'write) (list (LocalIdent'c)))))))

;;; Case Lambda

  (check-equal?
   (to-absyn/expand (expand #'(case-lambda
                                [(a b) (+ a b)]
                                [(a b c) (* a b c)])))
   (CaseLambda
    (list
     (PlainLambda '(a b) (list (PlainApp (ident #'+)
                                         (list (LocalIdent 'a) (LocalIdent 'b)))))
     (PlainLambda '(a b c) (list (PlainApp (ident #'*)
                                           (list (LocalIdent 'a)
                                                 (LocalIdent 'b)
                                                 (LocalIdent 'c))))))))

;;; Check module and provides

  (test-case "simple module"
    (define module-output
      (parameterize ([global-export-graph (hash)])
        (convert (expand
                  #'(module foo racket/base
                      (provide foo)
                      (define (foo name)
                        (write "Hello"))))
                 (build-path "/tmp/" "racketscript-test-expand.rkt"))))
    (check-equal? (Module-id module-output) 'foo)
    (check-equal? (Module-path module-output) (string->path "/tmp/racketscript-test-expand.rkt"))
    (check-equal? (Module-forms module-output)
                  (list
                   (list (SimpleProvide 'foo))
                   (DefineValues
                     '(foo)
                     (PlainLambda '(name)
                                  (list
                                   (PlainApp (ident #'write)
                                             (list (Quote "Hello")))))))))

  (check-equal? (parse-provide #'foo) (list (SimpleProvide 'foo)))
  (check-equal? (parse-provide #'(all-defined)) (list (AllDefined (set))))
  (check-equal? (parse-provide #'(all-defined-except foo bar))
                (list (AllDefined (set 'foo 'bar))))
  (check-equal? (parse-provide #'(rename foo bar))
                (list (RenamedProvide 'foo 'bar)))
  (check-equal? (parse-provide #'(prefix-all-defined foo))
                (list (PrefixAllDefined 'foo (set))))
  (check-equal? (parse-provide #'(prefix-all-defined-except foo add sub))
                (list (PrefixAllDefined 'foo (set 'add 'sub))))

  (test-case "check normal and prefix provides in module"
    (define module-output
      (parameterize ([global-export-graph (hash)])
        (convert (expand
                  #'(module foo racket/base
                      (provide (prefix-out f: (combine-out foo bar)))
                      (define foo #f)
                      (define bar #f)))
                 (build-path "/tmp/" "racketscript-test-expand.rkt"))))
    (check-equal? (Module-forms module-output)
                  (list
                   (list (RenamedProvide 'foo 'f:foo)
                         (RenamedProvide 'bar 'f:bar))
                   (DefineValues '(foo) (Quote #f))
                   (DefineValues '(bar) (Quote #f))))))
