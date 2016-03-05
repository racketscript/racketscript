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

(provide hash* global-config)

(define keep-srcloc (make-parameter #t))
(define current-phase (make-parameter 0))
;; FIXME: we really need a table for every phase, which means a table from phases to id tables
(define lexical-bindings (make-free-id-table))

(define (register-all! x)
  (cond [(list? x) (for-each register-all! x)]
        [(identifier? x) (dict-set! lexical-bindings x #t)]
        [(pair? x)
         (register-all! (car x))
         (register-all! (cdr x))]
        [(syntax? x) (register-all! (syntax-e x))]
        [else (error 'register-all! "unexpected ~a" x)]))

(define (identifier-binding* i)
  (if (dict-ref lexical-bindings i #f)
      'lexical
      (identifier-binding i)))

(define (hash-set* h . kvs)
  (let loop ([kvs kvs] [h h])
    (if (null? kvs)
        h
        (let* ([k (car kvs)]
               [v (cadr kvs)]
               [h (if v (hash-set h k v) h)])
          (loop (cddr kvs) h)))))

(define (hash* . kvs) (apply hash-set* (hash) kvs))

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
    (define stx* (namespace-syntax-introduce (expand stx)))
    (values stx* (do-post-expand stx* in-path))))

(define (do-post-expand stx in-path)
  (define i (make-syntax-introducer))
  (define m (i (datum->syntax
                #f
                `(module mod '#%kernel
                  (define-values (stx) (quote-syntax ,(i stx)))
                  (#%provide stx)))))
  (if (and in-path (keep-srcloc))
      (parameterize ([current-module-declare-name (make-resolved-module-path in-path)]
                     [current-module-declare-source in-path])
        (eval m)
        (dynamic-require in-path 'stx))
      stx))

(define current-module (make-parameter (list #f)))

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

(define (join-first x xs)
  (if (null? x) '()
    (append (car x) xs)))

;; Extract the information from a require statement that tells us how to find
;; the desired file.
;; This ensures that all path names are normalized.
(define (require-json v)
  (define (translate v)
    (let* ([str (symbol->string v)]
           [pre (substring str 0 (min 2 (string-length str)))])
      (if (string=? pre "#%")
        (list str)
        (resolve-module v))))
  (define (desym stx)
    (desymbolize (syntax-e stx)))
  (syntax-parse v
    [v:str        (list (resolve-module (syntax-e #'v)))]
    [s:identifier (list (translate (syntax-e #'s)))]
    [p #:when (path? (syntax-e #'p))
     (list (resolve-module (syntax-e #'p)))]
    [((~datum #%top) . x)
     (error 'never-happens)
     (list (resolve-module (syntax-e #'x)))]
    [((~datum rename) p _ ...) (require-json #'p)]
    [((~datum only) p _ ...) (require-json #'p)]
    [((~datum all-except) p _ ...) (require-json #'p)]
    [((~datum prefix) _ p) (require-json #'p)]
    [((~datum prefix-all-except) _ p _ ...) (require-json #'p)]
    [((~datum for-syntax) p ...) '()]
    [((~datum for-template) p ...) '()]
    [((~datum for-label) p ...) '()]
    [((~datum for-meta) 0 p ...)
     (append-map require-json (syntax->list #'(p ...)))]
    [((~datum for-meta) _ p ...) '()]
    [((~datum just-meta) 0 p ...)
     (append-map require-json (syntax->list #'(p ...)))]
    [((~datum just-meta) _ p ...) '()]
    [((~datum quote) s:id) (list (translate (syntax-e #'s)))]
    [((~datum file) s:str) (list (resolve-module (syntax-e #'s)))]
    [((~datum submod) path subs ...)
     (list (join-first (require-json #'path)
                       (map desym (syntax->list #'(subs ...)))))]
    ;; XXX May not be 100% correct
    [((~datum lib) path) (list (resolve-module (resolve-module-path `(lib ,(syntax-e #'path)) #f)))]
    [((~datum lib) _ ...) (error 'expand "`lib` multiple paths not supported")]
    [((~datum planet) _ ...)
     (error 'expand "`planet` require forms are not supported")]
    ))

(define quoted? (make-parameter #f))

(define global-config
  (let ()
    (define sysconfig
      (for/hash ([k '(collects-dir temp-dir init-dir pref-dir home-dir
                                   pref-file init-file config-dir addon-dir
                                   exec-file run-file sys-dir doc-dir orig-dir)])
        (values k (full-path-string (find-system-path k)))))
    (hash-set* sysconfig
               'version (version))))

(require syntax/id-table)
;; FIXME: we really need a table for every phase, which means a table from phases to id tables
(define table (make-free-id-table))
(define sym-table (make-hash))

(define (gen-name id)
  ;; use strings to make sure unreadablility isn't an issue
  (if (hash-ref sym-table (symbol->string (syntax-e id)) #f)
      (gensym (syntax-e id))
      (begin (hash-set! sym-table (symbol->string (syntax-e id)) #t)
             (syntax-e id))))


(define (id->sym id)
  (define sym (identifier-binding-symbol id))
  (define sym*
    (if (eq? 'lexical (identifier-binding id))
        (dict-ref! table id (λ _
                               ;(printf ">>> miss: ~a\n" id)
                               (gen-name id)))
        sym))
  ;(printf ">>> id sym sym*: ~a ~a ~a\n" id sym sym*)
  (symbol->string
   (if (quoted?)
       (syntax-e id)
       sym*)))

(define (flatten s s/loc)
  (let loop ([s s] [s/loc s/loc])
    (cond
      [(and (syntax? s) (pair? (syntax-e s)))
       (loop (syntax-e s) (syntax-e s/loc))]
      [(pair? s)
       (define-values (s* r* s*/loc r*/loc) (loop (cdr s) (cdr s/loc)))
       (values (cons (car s) s*)  r* (cons (car s/loc) s*/loc) r*/loc)]
      [(null? s)
       (values null #f null #f)]
      [else
       (values null s null s/loc)])))

(define (num n)
  (match n
    [(or +inf.0 -inf.0 +nan.0)
     (hash 'extended-real (number->string n))]
    [(? exact-integer?)
     (hash 'integer (~a n))]
    [(and (? real?) (? rational?) (? exact?) (not (? integer?)))
     (hash 'numerator (num (numerator n))
           'denominator (num (denominator n)))]
    [(? flonum?)
     (hash 'real n)]
    ;; FIXME
    [(? single-flonum?)
     (num (real->double-flonum n))]
    [(and (not (? real?)) (? complex?))
       (hash 'real-part (num (real-part n))
             'imag-part (num (imag-part n)))]))

(define (save-source-here? r)
  (or (hash-has-key? r 'quote-syntax)
      (hash-has-key? r 'variable-reference)
      (hash-has-key? r 'lambda)
      (hash-has-key? r 'case-lambda)))

(define (to-json v v/loc)
   (to-json* v v/loc))

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

(define (to-json* v v/loc)
  (define (proper l)
    (match l
      [(cons a b) (cons a (proper b))]
      [_ null]))
  (define (push-module path m)
    (define str (symbol->string m))
    (if (list? path)
      (append path (list str))
      (list path str)))
  (syntax-parse (list v v/loc)
                #:literal-sets ((kernel-literals #:phase (current-phase)))
    [(v:str _) (hash 'string (syntax-e #'v))]
    [(v _)
     #:when (path? (syntax-e #'v))
     (hash 'path (full-path-string (syntax-e #'v)))]
    ;; special case when under quote to avoid the "interesting"
    ;; behavior of various forms
    [((_ ...) _)
     #:when (quoted?)
     (map to-json (syntax->list v) (syntax->list v/loc))]
    [((_ . _) _)
     #:when (quoted?)
     (hash 'improper (list (map to-json (proper (syntax-e v)) (proper (syntax-e v/loc)))
                           (to-json (cdr (last-pair (syntax-e v))) (cdr (last-pair (syntax-e v/loc))))))]
    [((module name _ ...) _)
     (parameterize ([current-module (push-module (current-module) (syntax-e #'name))])
       (define-values (mod name body) (convert v v/loc #f))
       body)]
    [((module* name _ ...) _)
     (parameterize ([current-module (push-module (current-module) (syntax-e #'name))])
       (define-values (mod name body) (convert v v/loc #f))
       body)]
    [((#%declare _) _) #f] ;; ignore these
    ;; this is a simplification of the json output
    [_
     #:when (prefab-struct-key (syntax-e v))
     (let ([key (prefab-struct-key (syntax-e v))]
           [pats (cdr (vector->list (struct->vector (syntax-e v))))]
           [key* (prefab-struct-key (syntax-e v/loc))]
           [pats* (cdr (vector->list (struct->vector (syntax-e v/loc))))])
       (parameterize ([quoted? #t])
         (hash
          'prefab-key (to-json (datum->syntax #f key) (datum->syntax #f key*))
          'struct (map to-json pats pats*))))]
    [((#%plain-app e0 e ...)
      (#%plain-app e0* e* ...))
     (hash 'operator (to-json #'e0 #'e0*)
           'operands (map to-json
                          (syntax->list #'(e ...))
                          (syntax->list #'(e* ...))))]
    [((with-continuation-mark e0 e1 e2)
      (with-continuation-mark e0* e1* e2*))
     (hash 'wcm-key (to-json #'e0 #'e0*)
           'wcm-val (to-json #'e1 #'e1*)
           'wcm-body (to-json #'e2 #'e2*))]
    [((begin0 e0 e ...)
      (begin0 e0* e* ...))
     (hash 'begin0 (to-json #'e0 #'e0*)
           'begin0-rest (map to-json
                             (syntax->list #'(e ...))
                             (syntax->list #'(e* ...))))]
    [((if e0 e1 e2)
      (if e0* e1* e2*))
     (hash 'test (to-json #'e0 #'e0*)
           'then (to-json #'e1 #'e1*)
           'else (to-json #'e2 #'e2*))]
    [((let-values ([xs es] ...) b ...)
      (let-values ([xs* es*] ...) b* ...))
     (hash 'let-bindings (for/list ([x (syntax->list #'(xs ...))]
                                    [x* (syntax->list #'(xs* ...))]
                                    [e (syntax->list #'(es ...))]
                                    [e* (syntax->list #'(es* ...))])
                           (register-all! x)
                           (list (map id->sym (syntax->list x)) (to-json e e*)))
           'let-body (map to-json (syntax->list #'(b ...)) (syntax->list #'(b* ...))))]
    [((#%plain-lambda fmls . b)
      (#%plain-lambda fmls* . b*))
     (register-all! #'fmls)
     (hash 'lambda (to-json #'fmls #'fmls*)
           'body (map to-json
                      (syntax->list #'b)
                      (syntax->list #'b*)))]
    [((case-lambda (fmls . b) ...)
      (case-lambda (fmls* . b*) ...))
     (hash 'case-lambda
           (for/list ([fmls (syntax->list #'(fmls ...))]
                      [b (syntax->list #'(b ...))]
                      [fmls* (syntax->list #'(fmls* ...))]
                      [b* (syntax->list #'(b* ...))])
             (register-all! #'fmls)
             (hash 'lambda (to-json fmls fmls*)
                   'body (map to-json (syntax->list b) (syntax->list b*)))))]
    [((letrec-values ([xs es] ...) b ...)
      (letrec-values ([xs* es*] ...) b* ...))
     (hash 'letrec-bindings (for/list ([x (syntax->list #'(xs ...))]
                                       [e (syntax->list #'(es ...))]
                                       [x* (syntax->list #'(xs* ...))]
                                       [e* (syntax->list #'(es* ...))])
                              (register-all! x)
                              (list (map id->sym (syntax->list x)) (to-json e e*)))
           'letrec-body (map to-json (syntax->list #'(b ...)) (syntax->list #'(b* ...))))]
    [((quote e) (quote e*))
     (hash 'quote
           (parameterize ([quoted? #t])
             (to-json #'e #'e*)))]
    [((quote-syntax e _ ...)
      (quote-syntax e* _ ...))
     ;; XXX Ignore these mystical extra arguments for now.
     ;; Is this safe/reasonable?
     (hash 'quote-syntax
           (parameterize ([quoted? #t])
             (to-json #'e #'e*)))]
    [((define-values (i ...) b)
      (define-values (i* ...) b*))
     (hash 'define-values (map id->sym (syntax->list #'(i ...)))
           'define-values-body (to-json #'b #'b*)
           ;; keep these separately because the real symbols
           ;; may be unreadable extra symbols
           'define-values-names (map (compose symbol->string syntax-e)
                                     (syntax->list #'(i ...))))]
    [((define-syntaxes (i ...) b) _) #f]
    [((begin-for-syntax b ...)
      (begin-for-syntax b* ...))
     (hash 'begin-for-syntax
           (parameterize ([current-phase (add1 (current-phase))])
             (filter (λ (x) (or (is-module? x) (and (hash? x) (hash-has-key? x 'begin-for-syntax))))
                     (map to-json (syntax->list #'(b ...)) (syntax->list #'(b* ...))))))]

    [((#%require x ...) _)
     (let ([reqs (append-map require-json (syntax->list #'(x ...)))])
       (hash 'require reqs))]
    [((#%variable-reference) _)
     (hash 'variable-reference #f)]
    [((#%variable-reference id) (#%variable-reference id*))
     (hash 'variable-reference (to-json #'id #'id*))]
    [((_ ...) _)
     (map to-json (syntax->list v) (syntax->list v/loc))]
    [((#%top . x) _) (hash 'toplevel (symbol->string (syntax-e #'x)))]
    [((a . b) _)
     (let-values ([(s r s* r*) (flatten v v/loc)])
       (if r
           (hash 'improper (list (map to-json s s*) (to-json r r*)))
           (map to-json s s*)))]
    [(i:identifier _)
     (match (identifier-binding #'i (current-phase))
       ['lexical (hash 'lexical  (id->sym v))]
       [#f
        (hash 'toplevel (symbol->string (syntax-e v)))]
       [(list (app index->path (list src self?)) src-id nom-src-mod nom-src-id
                   src-phase import-phase nominal-export-phase)
        (define idsym (id->sym #'i))
        (define modsym (symbol->string (syntax-e v)))
        (hash* 'source-module (cond [(not src) 'null]
                                    [(and self? (path? (car src)))
                                     (cons (full-path-string (car src)) (cdr src))]
                                    [(path? src)
                                     (list (full-path-string src))]
                                    [(eq? src '#%kernel) #f] ;; omit these
                                    [(list? src) (list-module-path src)]
                                    [src (symbol-module-path src)]
                                    [else 'null])
               'module (if (string=? idsym modsym) #f modsym)
               'source-name (id->sym #'i)
               ;; currently ignored
               #;#;
               'phases (list src-phase import-phase nominal-export-phase))])]
    [(#(_ ...) _) (hash 'vector (map to-json (vector->list (syntax-e v)) (vector->list (syntax-e v/loc))))]
    [_ #:when (box? (syntax-e v))
       (hash 'box (to-json (unbox (syntax-e v)) (unbox (syntax-e v/loc))))]
    [_ #:when (boolean? (syntax-e v)) (syntax-e v)]
    [_ #:when (keyword? (syntax-e v)) (hash 'keyword (keyword->string (syntax-e v)))]
    ;; These numeric types seem excessive
    [_ #:when (extflonum? (syntax-e v))
       (hash 'number (num (extfl->inexact (syntax-e v))))]
    [_ #:when (number? (syntax-e v))
       (hash 'number (num (syntax-e v)))]
    [_ #:when (char? (syntax-e v))
       (hash 'char (~a (char->integer (syntax-e v))))]
    [_ #:when (regexp? (syntax-e v))
       (hash 'regexp (object-name (syntax-e v)))]
    [_ #:when (pregexp? (syntax-e v))
       (hash 'pregexp (object-name (syntax-e v)))]
    [_ #:when (byte-regexp? (syntax-e v))
       (hash 'byte-regexp (bytes->list (object-name (syntax-e v))))]
    [_ #:when (byte-pregexp? (syntax-e v))
       (hash 'byte-pregexp (bytes->list (object-name (syntax-e v))))]
    [_ #:when (bytes? (syntax-e v))
       (hash 'bytes (bytes->list (syntax-e v)))]
    [_ #:when (hash? (syntax-e v))
       (let ([ht (syntax-e v)]
             [ht* (syntax-e v/loc)])
         (parameterize ([quoted? #t])
           (hash 'hash-keys (to-json (datum->syntax #'lex (hash-keys ht))
                                     (datum->syntax #'lex (hash-keys ht*)))
                 'hash-vals (to-json (datum->syntax #'lex (hash-values ht))
                                     (datum->syntax #'lex (hash-values ht*))))))]
    ))

(define (is-module? m)
  (and (hash? m)
       (hash-has-key? m 'module-name)))

(define (convert mod mod/loc [config? #t])
  (syntax-parse (list mod mod/loc)
    #:literal-sets ((kernel-literals #:phase (current-phase)))
    [((module name:id lang:expr (#%plain-module-begin forms ...))
      (_ _ _                    (_ forms* ...)))
     (let ([lang-req (if (or (eq? (syntax-e #'lang) 'pycket)
                             (eq? (syntax-e #'lang) 'pycket/mcons)) ;; cheat in this case
                         (require-json #'#%kernel)
                         (require-json #'lang))])
       (parameterize ([current-phase 0])
         (values (symbol->string (syntax-e #'name))
                 (first lang-req)
                 (filter-map to-json
                             (syntax->list #'(forms ...))
                             (syntax->list #'(forms* ...))))))]
    [((module* name:id lang:expr (#%plain-module-begin forms ...))
      (_ _ _                    (_ forms* ...)))
     (let ([lang-req (cond
                       [(not (syntax-e #'lang)) (list #f)]
                       [(or (eq? (syntax-e #'lang) 'pycket)
                        (eq? (syntax-e #'lang) 'pycket/mcons)) ;; cheat in this case
                        (require-json #'#%kernel)]
                       [else (require-json #'lang)])])
       (define/with-syntax
         ((_ _ _ (_ s-forms ...))
          (_ _ _ (_ s-forms* ...)))
         (if (false? (syntax-e #'lang))
             (list (syntax-shift-phase-level mod (- (current-phase)))
                   (syntax-shift-phase-level mod/loc (- (current-phase))))
             (list mod mod/loc)))
       (parameterize ([current-phase 0])
         (values (symbol->string (syntax-e #'name))
                 (first lang-req)
                 (filter-map to-json
                                        (syntax->list #'(s-forms ...))
                                        (syntax->list #'(s-forms* ...))))))]
    [_
     (error 'convert "bad ~a ~a" mod (syntax->datum mod))]))

(define (read-module input)
  (read-syntax (object-name input) input))

(define (open-read-module in-path)
  (read-module (open-input-file in-path)))

(module+ main
  (read-accept-reader #t)
  (read-accept-lang #t)

  (define in-path (build-path (current-directory) "main.rkt"))
  (define mod (open-read-module in-path))
  (define-values  (expanded expanded-srcloc) (do-expand mod in-path))
  (parameterize ([keep-srcloc srcloc?])
    (define-values (mod lang body) (convert expanded expanded-srcloc))
    (displayln lang)
    (displayln mod)
    (displayln body)))

