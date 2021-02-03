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
         racket/sequence
         racket/syntax
         racket/vector
         syntax/modresolve
         syntax/stx
         syntax/parse
         syntax/id-table
         version/utils
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
         read-and-expand-module
         quick-expand)

(define current-module (make-parameter #f))
(define current-phase (make-parameter 0))
(define quoted? (make-parameter #f))
(define lexical-bindings (make-parameter #f)) ;; Free-Id-Table
(define defined-names (make-parameter #f)) ;; set of symbols, rkt prog can have defs with same name
(define dup-names (make-parameter #f)) ;; free-id-table, rkt prog can have defs with same name
(define skip-freshening? (make-parameter #f))

;;;----------------------------------------------------------------------------
;;;; Module dependencies and imports

;; (Setof (U ModulePath Symbol))
(define current-module-imports (make-parameter (set)))

;;;----------------------------------------------------------------------------
;;;; Module paths

(define (index->path i)
  (define-values (v u) (module-path-index-split i))
  (if v
      (list (resolved-module-path-name (module-path-index-resolve i)) #f)
      (list (current-module) #t)))

;;;-----------------------------------------------------------------------------
;;;; Conversion and expansion

;; record all (symbolic) names defined via define-values
;; - need to track these because Racket progs can have multiple defs with same name
(define (register-defined-names! stx)
  (cond
    [(stx-list? stx)
     (for-each register-defined-names! (syntax->list stx))]
    [(identifier? stx)
     (if (set-member? (defined-names) (syntax-e stx))
         (register-dup-name! stx) ; add to dup-names set if already seen
         (set-add! (defined-names) (syntax-e stx)))] ; else add to defined-names
    [(stx-pair? stx)
     (register-defined-names! (stx-car stx))
     (register-defined-names! (stx-cdr stx))]
    [else (error 'register-defined-names "unexpected ~a" stx)]))

;; registers duplicate names that need to be freshened in the next pass
(define (register-dup-name! id)
  (dict-set! (dup-names) id (generate-temporary id)))

(define (register-lexical-bindings! stx)
  (cond
    [(stx-list? stx)
     (for-each register-lexical-bindings! (syntax->list stx))]
    [(identifier? stx)
     (dict-set! (lexical-bindings)
                stx
                (if (skip-freshening?)
                    stx
                    (car (generate-temporaries (list stx)))))]
    [(stx-pair? stx)
     (register-lexical-bindings! (stx-car stx))
     (register-lexical-bindings! (stx-cdr stx))]
    [else (error 'register-lexical-bindings "unexpected ~a" stx)]))

(define (get-freshened-lexical-binding! id)
  (dict-ref! (lexical-bindings)
             id
             (λ _ (error 'get-freshened-lexical-binding! "Missed binding: ~a" id))))

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

(define (formals->absyn formals)
  (cond
    [(stx-list? formals) (stx-map formals->absyn formals)]
    [(stx-pair? formals)
     ;; Splits the formals to be compatible with
     ;; LetValues/PlainLambda.  If we reach here, rest of the
     ;; structure will also reach here unless its terminal. We build
     ;; up proper part and improper part result recursively.
     (cond
       [(stx-pair? (stx-cdr formals))
        (match-define (cons pos rst) (formals->absyn (stx-cdr formals)))
        (cons (cons (formals->absyn (stx-car formals)) pos)
              rst)]
       [else (cons (list (formals->absyn (stx-car formals)))
                   (formals->absyn (stx-cdr formals)))])]
    [(identifier? formals)
     (syntax-e (get-freshened-lexical-binding! formals))]
    [(null? formals) null]
    [else 'formals->absyn "Invalid formals: ~a" formals]))

(define (to-absyn v)
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
     (register-lexical-bindings! #'(xs ...))
     (LetValues (for/list ([x (syntax-e #'(xs ...))]
                           [e (syntax->list #'(es ...))])
                  (cons (formals->absyn x)
                        (to-absyn e)))
                (map to-absyn (syntax->list #'(b ...))))]
    [(letrec-values ([xs es] ...) b ...)
     (register-lexical-bindings! #'(xs ...))
     (LetValues (for/list ([x (syntax->list #'(xs ...))]
                           [e (syntax->list #'(es ...))])
                  (cons (formals->absyn x)
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
                   [(formals . body)
                    (register-lexical-bindings! #'formals)
                    (PlainLambda (formals->absyn #'formals)
                                 (stx-map to-absyn #'body)
                                 #f)]))
               #'clauses))]
    [(#%plain-lambda formals . body)
     (register-lexical-bindings! #'formals)
     (define unchecked? (syntax-property v 'racketscript-unchecked-lambda?))
     (define fabsyn (formals->absyn #'formals))
     (PlainLambda fabsyn (map to-absyn (syntax->list #'body)) unchecked?)]
    [(define-values (name)
       (#%plain-app (~datum #%js-ffi) (quote (~datum require)) (quote mod:str)))
     ;; HACK: Special case for JSRequire
     (JSRequire (syntax-e #'name) (syntax-e #'mod) 'default)]
    [(define-values (name)
       (#%plain-app (~datum #%js-ffi) (quote (~datum require)) (quote *) (quote mod:str)))
     ;; HACK: Special case for JSRequire
     (JSRequire (syntax-e #'name) (syntax-e #'mod) '*)]
    [(define-values (id ...) b)
     (DefineValues (syntax->datum #'(id ...)) (to-absyn #'b))]
    [(#%top . x) (TopId (syntax-e #'x))]
    [(#%variable-reference x) (VarRef (to-absyn #'x))]
    [(#%variable-reference) (VarRef #f)]
    [i:identifier #:when (quoted?) (syntax-e #'i)]
    [i:identifier
     (define (rename-module mpath)
       ;; Rename few modules for simpler compilation
       (cond
         [(symbol? mpath) (list #f mpath)]
         #;[(collects-module? mpath) (list #t '#%kernel)]
         [else (list #f mpath)]))

     (match (identifier-binding #'i)
       ['lexical (LocalIdent (syntax-e (get-freshened-lexical-binding! #'i)))]
       [#f (TopLevelIdent (syntax-e #'i))]
       [(list src-mod src-id nom-src-mod mod-src-id src-phase import-phase nominal-export-phase)
        ;; from where we import
        (match-define (list src-mod-path-orig self?) (index->path src-mod))
        (match-define (list nom-src-mod-path-orig _) (index->path nom-src-mod))
        (match-define (list module-renamed? src-mod-path) (rename-module src-mod-path-orig))

        (cond
          [self? (LocalIdent (syntax-e #'i))]
          [else
           ;; Add the module from where we actual import this, so that we import this, and
           ;; any side-effects due to this module is actually executed
           ;(match-define (list nom-mod-path _) (index->path nom-src-mod))
           ;(current-module-imports (set-add (current-module-imports) nom-mod-path))

           ;; And still add the actual module where identifier is defined for easy
           ;; and compact import. NOTE:In future we may want to remove this and
           ;; compute this with moddeps information.
           ;; FIXME?: Racket7 workaround
           (if (and (version<=? "7.0" (version))
                    (equal? src-mod-path '#%runtime))
               (current-module-imports (set-add (current-module-imports) '#%kernel))
               (current-module-imports (set-add (current-module-imports) src-mod-path)))

           ;;HACK: See test/struct/import-struct.rkt. Somehow, the
           ;;  struct contructor has different src-id returned by
           ;;  identifier-binding than the actual identifier name used
           ;;  at definition site. Implicit renaming due to macro
           ;;  expansion?
           ;;
           ;;HACK: See tests/modules/rename-and-import.rkt and
           ;;  tests/rename-from-primitive.rkt. When importing from a
           ;;  module with a rename, identifier-binding's, mod-src-id
           ;;  shows the renamed id, i.e. the one it is imported as
           ;;  instead of what it is exported as. We handle this
           ;;  special case where both src-mod and nom-src-mod are
           ;;  equal. If both source module and normalized module are
           ;;  same with different ids:
           ;;  - Check if nom-src-id is exported and use that. Or,
           ;;  - Check if src-id is exported and use that. Or,
           ;;  - Fallback to module source id.
           (define-values (id-to-follow path-to-symbol)
             (cond
               [(and (equal? src-mod nom-src-mod)
                     (not (equal? src-id mod-src-id)))
                (let ([path-to-symbol-src (follow-symbol (global-export-graph)
                                                         nom-src-mod-path-orig
                                                         mod-src-id)])
                  (if path-to-symbol-src
                      (values mod-src-id path-to-symbol-src)
                      (let ([path-to-symbol-nom (follow-symbol (global-export-graph)
                                                               nom-src-mod-path-orig
                                                               src-id)])
                        (if path-to-symbol-nom
                            (values src-id path-to-symbol-nom)
                            (values mod-src-id #f)))))]
               [else (values mod-src-id
                             (follow-symbol (global-export-graph)
                                            nom-src-mod-path-orig
                                            mod-src-id))]))

           ;; If the moduele is renamed use the id name used at the importing
           ;; module rather than defining module. Since renamed, module currently
           ;; are #%kernel which we write ourselves in JS we prefer original name.
           ;; TODO: We potentially might have clashes, but its unlikely.
           (define-values (effective-id effective-mod reachable?)
             (cond
               [module-renamed? (values mod-src-id src-mod-path #t)]
               [(false? path-to-symbol)
                (when (and (not (ignored-undefined-identifier? #'i))
                           (symbol? src-mod-path))
                  ;; Since free id's are anyway caught by Racket, just
                  ;; complain about the primitives.
                  (log-rjs-warning
                   "Implementation of identifier ~a not found in module ~a!"
                   (syntax-e #'i) src-mod-path))
                (values id-to-follow src-mod-path #f)]
               [else
                (match-let ([(cons (app last mod) (? symbol? id)) path-to-symbol])
                  (values id mod #t))]))

           (ImportedIdent effective-id effective-mod reachable?)])])]
    [(define-syntaxes (i ...) b) #f]
    [(set! s e)
     (let ([id* (if (equal? (identifier-binding #'s) 'lexical)
                    (get-freshened-lexical-binding! #'s)
                    #'s)])
       (Set! (syntax-e id*) (to-absyn #'e)))]
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
    [_ #:when (complex? (syntax-e v)) #f]
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

(define (freshen-form v)
  (syntax-parse v
    #:literal-sets ((kernel-literals #:phase (current-phase)))
    [(define-values (name) (#%plain-app (~datum #%js-ffi) . _))
     ;; HACK: Special case for JSRequire
     this-syntax]
    [(define-values (id ...) . _)
     (register-defined-names! #'(id ...))
     this-syntax]
    [_ this-syntax]))

(define (convert mod path)
  (syntax-parse (freshen-mod-forms mod) ; Racket can have 2+ defs with same name
    #:literal-sets ((kernel-literals #:phase (current-phase)))
    [(module name:id lang:expr (#%plain-module-begin forms ...))
     (parameterize ([current-module path]
                    [current-module-imports (set)]
                    [current-directory (path-only path)]
                    [lexical-bindings (make-free-id-table)])
       (define mod-id (syntax-e #'name))
       (log-rjs-info "[absyn] ~a" mod-id)
       (let* ([ast (filter-map to-absyn (syntax->list #'(forms ...)))]
              [imports (current-module-imports)]
              [quoted-bindings (list->set
                                (map
                                 syntax-e
                                 (filter
                                  (λ (x)
                                    ;; We just compile phase 0 forms now
                                    (let ([r (identifier-binding x)])
                                      (and (list? r)
                                           (zero? (list-ref (identifier-binding x) 4)))))
                                  (get-quoted-bindings #'(forms ...)))))])
         (Module mod-id
                 path
                 (syntax->datum #'lang)
                 imports
                 quoted-bindings
                 ast)))]
    [_
     (error 'convert "bad ~a ~a" mod (syntax->datum mod))]))

(define (freshen-mod-forms mod)
  (syntax-parse mod
    #:literal-sets ((kernel-literals #:phase (current-phase)))
    [(module name:id lang:expr (#%plain-module-begin forms ...))
     (parameterize ([defined-names (mutable-set)]
                    [dup-names (make-free-id-table)])
       (define mod-id (syntax-e #'name))
       (log-rjs-info "[freshening module forms] ~a" mod-id)
       (for-each freshen-form (syntax->list #'(forms ...)))
       (replace-dup-names this-syntax))]
    [_ (error 'freshen-mod-forms "bad ~a ~a" mod (syntax->datum mod))]))

;; replace all ids in (dup-names) with fresh name;
;; handles modules that have multiple defs with the same symbolic name
(define (replace-dup-names stx)
  (cond
    [(pair? stx) (cons (replace-dup-names (car stx))
                       (replace-dup-names (cdr stx)))]
    [(stx-pair? stx)
     (datum->syntax stx (cons (replace-dup-names (stx-car stx))
                              (replace-dup-names (stx-cdr stx)))
                    stx stx stx)]
    [(and (identifier? stx) (dict-has-key? (dup-names) stx))
     (dict-ref (dup-names) stx)]
    [else stx]))

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
  (call-with-input-file (actual-module-path in-path)
    (λ (in)
      (read-module in))))

(define (quick-expand in-path)
  (log-rjs-info "[expand] ~a" in-path)
  (read-accept-reader #t)
  (read-accept-lang #t)
  (define full-path (path->complete-path (actual-module-path in-path)))
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
;;; Flatten Phases in Module

(define (flatten-module-forms mod-stx)
  (define phase-forms (make-hash))

  (define (save-form! form)
    (hash-update! phase-forms
                  (current-phase)
                  (λ (v)
                    (append v (list form)))
                  '()))

  (define (walk stx)
    (syntax-parse stx
      #:literal-sets ((kernel-literals #:phase (current-phase)))
      [((begin-for-syntax forms ...) . tl)
       (parameterize ([current-phase (add1 (current-phase))])
         (walk #'(forms ...)))
       (walk #'tl)]
      [(hd . tl)
       (save-form! #'hd)
       (walk #'tl)]
      [() (void)]
      [_ (save-form! stx)]))

  (parameterize ([current-phase 0])
    (walk mod-stx))

  (for/hash ([(phase forms) phase-forms])
    (values phase (datum->syntax mod-stx forms))))

;;;----------------------------------------------------------------------------
;;; Prepare binding dependency graph

(define (get-quoted-bindings stx)
  (syntax-parse stx
    #:literal-sets ((kernel-literals #:phase (current-phase)))
    [(quote-syntax e)
     (parameterize ([quoted? #t]
                    [current-phase (sub1 (current-phase))])
       (get-quoted-bindings #'e))]
    [(define-syntaxes _ b)
     (parameterize ([current-phase (add1 (current-phase))])
       (get-quoted-bindings #'b))]
    [(begin-for-syntax forms ...)
     (parameterize ([current-phase (add1 (current-phase))])
       (get-quoted-bindings #'(forms ...)))]
    [(hd . tl)
     (append (get-quoted-bindings #'hd)
             (get-quoted-bindings #'tl))]
    [() '()]
    [v:id #:when (quoted?)
          (match (identifier-binding #'v (current-phase))
            [(list src-mod src-id _ _ src-phase _ _)
             (define-values (v u) (module-path-index-split src-mod))
             (cond
               [(and (false? v) (false? u))
                (unless (equal? src-phase (current-phase))
                  (error 'get-quoted-binding
                         "Identifier phase is ~a, expecte ~a."
                         src-phase (current-phase)))
                (list stx)]
               [v '()])]
            [_ '()])]
    [_ '()]))

;;;----------------------------------------------------------------------------

(module+ test
  (require rackunit
           "util.rkt"
           "moddeps.rkt")
  (define-syntax-rule (to-absyn/expand stx)
    (parameterize ([global-export-graph (hash)]
                   [skip-freshening? #t]
                   [lexical-bindings (make-free-id-table)])
      (to-absyn/top (expand stx))))
  (define (ident i)
    (match (identifier-binding i)
      ['lexical (LocalIdent (syntax-e i))]
      [#f (TopLevelIdent (syntax-e i))]
      [(list mod-path mod-id _ _ _ _ _)
       (match-define (list mod-path* _) (index->path mod-path))
       ;;TODO: Just considering unreachable idents for tests here.
       (ImportedIdent mod-id mod-path* #f)]))

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

  (check-equal? (to-absyn/expand #'displayln)
                (ident #'displayln))

  ;; Check lambdas

  (check-equal? (to-absyn/expand #`(λ (x) x))
                (PlainLambda '(x) (list (LocalIdent 'x)) #f))
  (check-equal? (to-absyn/expand #`(λ x x))
                (PlainLambda 'x (list (LocalIdent 'x)) #f))

  ;; if we start with empty global-export-graph (above),
  ;; src of prims, e.g., #'+, will be #%runtime (from src-mod part of identifier-binding);
  ;; - but loading global-export-graph to be racket/base and following it for #'+
  ;;   leadsx to #%kernel
  (check-equal? (to-absyn/expand #`(λ (a b . c) (+ a b (reduce + c))))
                (PlainLambda
                 '((a b) . c)
                 (list
                  (PlainApp (ident #'+)
                            (list (LocalIdent 'a) (LocalIdent'b)
                                  (PlainApp (TopId 'reduce)
                                            (list (ident #'+) (LocalIdent 'c))))))
                 #f))
  ;; Check application

  (check-equal? (to-absyn/expand #`(print "hello"))
                (PlainApp (ident #'print) (list (Quote "hello"))))
  (check-equal? (to-absyn/expand #`((λ (x) x) 42))
                (PlainApp (PlainLambda '(x) (list (LocalIdent 'x)) #f)
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
                                                (list (LocalIdent 'n)))))))))
                   #f)))
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
                       (list (PlainApp (ident #'sub1) (list (LocalIdent 'n)))))))))
         #f)
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
                       (list (PlainApp (ident #'sub1) (list (LocalIdent 'n)))))))))
         #f)))))
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
       (PlainApp (ident #'write) (list (LocalIdent'c))))
      #f)))

;;; Case Lambda

  (check-equal?
   (to-absyn/expand (expand #'(case-lambda
                                [(a b) (+ a b)]
                                [(a b c) (* a b c)])))
   (CaseLambda
    (list
     (PlainLambda '(a b) (list (PlainApp (ident #'+)
                                         (list (LocalIdent 'a) (LocalIdent 'b))))
                  #f)
     (PlainLambda '(a b c) (list (PlainApp (ident #'*)
                                           (list (LocalIdent 'a)
                                                 (LocalIdent 'b)
                                                 (LocalIdent 'c))))
                  #f))))

  (require version/utils)
  (define kernel-lang (if (version<? (version) "7.0") '#%kernel '#%runtime))
;;; Check freshening
  (test-case "test freshening"
    ;;TODO: We need to check alpha-equivalence here rather than hardcoding
    ;;      renamed identifiers which can vary based on run.
    (parameterize ([global-export-graph (hash)]
                   [lexical-bindings (make-free-id-table)]
                   [skip-freshening? #f])
      (define expand-to-absyn (compose1 to-absyn/top expand))
      (check-equal? (expand-to-absyn #'(λ (x) x))
                    (PlainLambda '(x1) (list (LocalIdent 'x1)) #f))
      (check-equal? (expand-to-absyn #'(let-values ([(x) 1]
                                                    [(y) 2])
                                         (list x y)
                                         (let-values ([(x) 3]
                                                      [(z) 4])
                                           (list x y z)
                                           (let-values ([(y) 6])
                                             (list x y z)))))
                    (LetValues
                     (list (cons '(x2) (Quote 1)) (cons '(y3) (Quote 2)))
                     (list
                      (PlainApp
                       (ImportedIdent 'list kernel-lang #f)
                       (list (LocalIdent 'x2) (LocalIdent 'y3)))
                      (LetValues
                       (list (cons '(x4) (Quote 3)) (cons '(z5) (Quote 4)))
                       (list
                        (PlainApp
                         (ImportedIdent 'list kernel-lang #f)
                         (list (LocalIdent 'x4) (LocalIdent 'y3) (LocalIdent 'z5)))
                        (LetValues
                         (list (cons '(y6) (Quote 6)))
                         (list
                          (PlainApp
                           (ImportedIdent 'list kernel-lang #f)
                           (list (LocalIdent 'x4) (LocalIdent 'y6) (LocalIdent 'z5))))))))))

      (check-equal? (expand-to-absyn #'(λ (x y z)
                                         (list x y z (λ (x)
                                                       (list x y z (λ (y)
                                                                     (list x y z)))))))
                    (PlainLambda
                     '(x7 y8 z9)
                     (list
                      (PlainApp
                       (ImportedIdent 'list kernel-lang #f)
                       (list
                        (LocalIdent 'x7)
                        (LocalIdent 'y8)
                        (LocalIdent 'z9)
                        (PlainLambda
                         '(x10)
                         (list
                          (PlainApp
                           (ImportedIdent 'list kernel-lang #f)
                           (list
                            (LocalIdent 'x10)
                            (LocalIdent 'y8)
                            (LocalIdent 'z9)
                            (PlainLambda
                             '(y11)
                             (list
                              (PlainApp
                               (ImportedIdent 'list kernel-lang #f)
                               (list
                                (LocalIdent 'x10)
                                (LocalIdent 'y11)
                                (LocalIdent 'z9))))
                             #f))))
                         #f))))
                     #f)
                    #f)))

;;; Check module and provides

  (test-case "simple module"
    (define module-output
      (parameterize ([global-export-graph (hash)]
                     [skip-freshening? #t])
        (convert (expand
                  #'(module foo racket/base
                      (provide foo)
                      (define (foo name)
                        (print "Hello"))))
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
                                   (PlainApp (ident #'print)
                                             (list (Quote "Hello"))))
                                  #f)))))

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
                   (DefineValues '(bar) (Quote #f)))))

;;; Check module flattening
  (test-case "check flattening of module by splitting as per phases"
    (define (flatten-module-forms->datum forms)
      (for/hash ([(k v) (flatten-module-forms forms)])
        (values k (syntax->datum v))))

    (define (flatten-module->datum mod-stx)
      (syntax-parse mod-stx
        [(module name lang
           (#%plain-module-begin forms ...))
         (flatten-module-forms->datum #'(forms ...))]))

    #;(check-equal? (flatten-module->datum
                   #'((begin-for-syntax
                        (begin-for-syntax
                          "Phase 2.1"
                          (begin-for-syntax "Phase 3.1")
                          "Phase 2.2"
                          (begin-for-syntax "Phaes 3.2"))
                        "Phase 1.1"
                        (begin-for-syntax "Phase 2.3"))))
                  "")

    (define test-mod-1
      (expand
       #'(module foo racket/base
           (require (for-meta 1 racket/base)
                    (for-meta 2 racket/base)
                    (for-meta 3 racket/base))
           (define-values (foo) "Foo")
           (begin-for-syntax
             (define-values (foo-1) (λ () (display "Foo Phase 1"))))
           (define-values (bar) (λ () "Bar"))
           (begin-for-syntax
             (define-values (bar-1) (λ () (display "Bar Phase 1")))
             (begin-for-syntax
               (define-values (foo-2) (λ () "Foo Phase 2")))
             (define-values (foobar-1) (λ () "FooBar Phase 1")))
           (begin-for-syntax
             (begin-for-syntax
               (begin-for-syntax
                 (define-values (foo-3) (λ () (displayln "Foo Phase 3")))))))))

    (check-equal?
     (flatten-module->datum test-mod-1)
     '#hash((0
             .
             ((module configure-runtime '#%kernel
                (#%module-begin
                 (#%require racket/runtime-config)
                 (#%app configure '#f)))
              (#%require (for-meta 1 racket/base))
              (#%require (for-meta 2 racket/base))
              (#%require (for-meta 3 racket/base))
              (define-values (foo) '"Foo")
              (define-values (bar) (lambda () '"Bar"))))
            (1
             .
             ((define-values (foo-1) (lambda () (#%app display '"Foo Phase 1")))
              (define-values (bar-1) (lambda () (#%app display '"Bar Phase 1")))
              (define-values (foobar-1) (lambda () '"FooBar Phase 1"))))
            (2 . ((define-values (foo-2) (lambda () '"Foo Phase 2"))))
            (3
             .
             ((define-values (foo-3) (lambda () (#%app displayln '"Foo Phase 3"))))))))

  (test-case "get quoted bindings in module"
    (define test-mod-1
      (expand
       #'(module foo racket
           (define (internal-func)
             (displayln "Hello World!"))
           (define-syntax foo
             (λ (stx) #'(internal-func))))))

    (check-equal? (map syntax-e (get-quoted-bindings test-mod-1))
                  '(internal-func))))

