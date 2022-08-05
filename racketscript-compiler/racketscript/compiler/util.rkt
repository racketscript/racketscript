#lang racket/base

(require (for-syntax racket/base)
         "config.rkt"
         "ident.rkt"
         "util-untyped.rkt"
         "set.rkt"
         "match.rkt"
         "anaphoric.rkt"
         racket/flonum
         racket/fixnum
         (only-in racket/list range)
         (only-in racket/string string-prefix?)
         (only-in racket/path find-relative-path))

(provide hash-set-pair*
         improper->proper
         flatten1
         append1
         split-before-last
         for/fold/last
         for/last?
         reverse-pair
         assocs->hash-list
         module-path->name
         collects-module?
         module-output-file
         module->relative-import
         actual-module-path
         js-identifier?
         jsruntime-import-path
         path-parent
         length=?
         string-slice
         log
         converge
         override-module-path
         simple-module-path
         primitive-module?
         primitive-module-path?
         ++
         ~a
         sequence-length
         (all-from-out "ident.rkt"))

(module+ test (require rackunit))


(define ++ string-append)

(define simple-form-path (compose simplify-path path->complete-path))

(define (~a . args)
  (let* ([fmt-lst (build-list (length args) (λ (_) "~a"))]
         [fmt-string (apply string-append fmt-lst)])
    (apply format fmt-string args)))

;; taken from racket/collects/racket/syntax.rkt
(define (format-symbol fmt . args)
  (define (->string x err)
    (cond [(string? x) x]
          [(symbol? x) (symbol->string x)]
          [(identifier? x) (symbol->string (syntax-e x))]
          [(keyword? x) (keyword->string x)]
          [(number? x) (number->string x)]
          [(char? x) (string x)]
          [else (raise-argument-error err
                                      "(or/c string? symbol? identifier? keyword? char? number?)"
                                      x)]))

  (define (restricted-format-string? fmt)
    (regexp-match? #rx"^(?:[^~]|~[aAn~%])*$" fmt))

  (define (check-restricted-format-string who fmt)
    (unless (restricted-format-string? fmt)
      (raise-arguments-error who
                            "format string should have only ~a placeholders"
                            "format string" fmt)))

  (define (convert x) (->string x 'format-symbol))

  (check-restricted-format-string 'format-symbol fmt)
  (let ([args (map convert args)])
    (string->symbol (apply format fmt args))))


;; taken from racket/collects/racket/path.rkt
(define (path-only name)
  (unless (or (path-string? name)
              (path-for-some-system? name))
    (raise-argument-error 'path-only "(or/c path-string? path-for-some-system?)" name))
  (let-values ([(base file dir?) (split-path name)])
    (cond [dir? (if (string? name) (string->path name) name)]
          [(path-for-some-system? base) base]
          [else #f])))

(define (simple-module-path mod)
  (if (symbol? mod)
      mod
      (simple-form-path mod)))

(define (string-slice str start [end #f])
  (let* ([len (string-length str)]
         [fix-index (λ (i) (if (negative? i) (+ len i) i))]
         [start (fix-index start)]
         [end (fix-index (or end len))])
    (substring str start end)))

;; Because `path-only` return type is `path-for-some-system` and that
;; is not in any way helping
(define (path-parent p)
  (define p* (path-only p))
  (if (path? p*)
      p*
      (error 'path-parent "No parent for ~a" p)))

(define (length=? lst n)
  (equal? (length lst) n))

(module+ test
  (check-false (length=? '() 1))
  (check-true (length=? '(1 2) 2)))

(define (reverse-pair p)
  (cons (cdr p) (car p)))

;; Flatten a list of list upto one level
(define (flatten1 lst)
  (foldl append '() lst))

(define (append1 lst a)
  (append lst (list a)))



;; taken and modified from collects/racket/list.rkt
(define (split-at-right list n)
  (define (drop* list n)
    (if (zero? n) list (and (pair? list) (drop* (cdr list) (sub1 n)))))

(define (too-large who list n)
  (define proper? (list? list))
  (raise-argument-error who
                        (format "a ~alist with at least ~a ~a"
                                (if proper? "" "(possibly improper) ")
                                n
                                (if proper? "elements" "pairs"))
                        list))

  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'split-at-right "exact-nonnegative-integer?" 1 list n))

  (let loop ([list list]
             [lead (or (drop* list n) (too-large 'split-at-right list n))]
             [pfx '()])
    (if (pair? lead)
      (loop (cdr list) (cdr lead) (cons (car list) pfx))
      (values (reverse pfx) list))))

;; Returns lst with its last element and the last element
(define (split-before-last lst)
  (define-values (ls list-v) (split-at-right lst 1))
  (match list-v
    [`(,v) (values ls v)]))

;; Update given hash `h` with given list of (key, value) pairs.
(define (hash-set-pair* h pairs)
  (let loop ([p* pairs] [h h])
    (if (null? p*)
        h
        (let* ([p (car p*)]
               [k (car p)]
               [v (cdr p)])
          (loop (cdr p*) (hash-set h k v))))))

(module+ test
  (check-equal? (hash-set-pair* (hash) '()) (hash))
  (check-equal? (hash-set-pair* (hash) (list '(a . b) '(c . d)))
                (hash 'a 'b 'c 'd)))

;; Takes a list of (key, value) pairs `assocs` and returns a hash
;; with all values pointed by same key folded into a list
(define (assocs->hash-list assocs)
  (define empty-hash (hash))
  (foldl (λ (a* result)
           (let ([key (car a*)]
                 [val (cdr a*)])
             (hash-update result key
                          (λ (v) (cons val v))
                          (λ () '()))))
         empty-hash
         assocs))

(module+ test
  (check-equal?
   (assocs->hash-list '((a . b) (b . c) (c . d) (a . e) (c . f) (a . g)))
   (hash 'a '(g e b)
         'b '(c)
         'c '(f d))))


;;; Paths that we use every now and then --------------------------------------

(define (module-path->name mod-name)
  (cond
    [(symbol? mod-name) (jsruntime-module-path mod-name)] ;; #%kernel, #%utils
    [(path? mod-name) mod-name]
    [else (error 'module-path->name
                 "Don't know how to translate module name '~a'" mod-name)]))

(define (main-source-directory)
  (path-parent (main-source-file)))

(define (jsruntime-import-path base runtime-fpath)
  ;; TODO: Make runtime, modules, and everything united!
  (define (fix-for-down p)
    (define p-str (~a p))
    (if (string-prefix? p-str "..")
        p
        (build-path (~a "./" p-str))))
  (fix-for-down
   (find-relative-path (path-parent (module-output-file base))
                             runtime-fpath)))

;;; Module path renaming ------------------------------------------------------

(define (actual-module-path in-path)
  (cond
    [(path? in-path) in-path]
    [(and (symbol? in-path) (primitive-module? in-path))
     (build-path racketscript-runtime-dir
                 (~a (substring (symbol->string in-path) 2) ".rkt"))]
    [else (error 'actual-module-path "~a is not a primitive module" in-path)]))

(define (primitive-module? mod)
  (set-member? primitive-modules mod))

(define (primitive-module-path? mod-path)
  (let* ([primitive-modules-paths (set-map primitive-modules
                                           (λ (m)
                                             (cons (actual-module-path m) m)))]
         [result (assoc mod-path primitive-modules-paths)])
    (if (pair? result)
        (cdr result)
        #f)))

(define (override-module-path mod)
  (if (symbol? mod)
      (build-path racketscript-runtime-dir
                  (~a (substring (symbol->string mod) 2) ".rkt"))
      mod))

;; taken from racket/collects/racket/file.rkt
(define (make-directory* dir)
  (unless (path-string? dir)
    (raise-argument-error 'make-directory* "path-string?" dir))
  (let-values ([(base name dir?) (split-path dir)])
    (when (and (path? base)
               (not (directory-exists? base)))
      (make-directory* base))
    (unless (directory-exists? dir)
      (with-handlers ([exn:fail:filesystem:exists? void])
        (make-directory dir)))))

;; NOTE: returns simplified path, which is required by fns like find-relative-path
(define (module-output-file mod)
  (simple-form-path
    (match (module-kind mod)
      [`(primitive ,mod-path)
      ;; Eg. #%kernel, #%utils ...
       (build-path (output-directory)
                   "runtime"
                   (~a (substring (symbol->string mod-path) 2) ".rkt.js"))]
      [`(runtime ,rel-path)
       (build-path (output-directory) "runtime" (~a rel-path ".js"))]
      [`(collects ,base ,rel-path)
       (build-path (output-directory) "collects" (~a rel-path ".js"))]
      [`(links ,name ,root-path ,rel-path)
       (define output-path
         (build-path (output-directory) "links" name (~a rel-path ".js")))
       ;; because we just created root links directory, but files could be
       ;; deep arbitrarily inside
       (make-directory* (path-only output-path))
       ;; TODO: doesn't handle arbitrary deep files for now
       output-path]
      [`(general ,mod-path)
       (let* ([main (main-source-file)]
               [rel-path (find-relative-path (simple-form-path (path-parent main))
                                             (simple-form-path mod-path))])
         (build-path (output-directory) "modules" (~a rel-path ".js")))])))

(define (module->relative-import mod-path)
  ;; ES6 modules imports need "./" prefix for importing relatively
  ;; to current module, rather than relative to main module. Weird :/
  (define (fix-for-down p)
    (define p-str (~a p))
    (if (string-prefix? p-str "..")
        p
        (build-path (~a "./" p-str))))

  (let ([src (current-source-file)])
    (if src
        (fix-for-down
         (find-relative-path (path-parent (module-output-file src))
                                   (module-output-file mod-path)))
         (error 'module->relative-import "current-source-file is #f"))))

(define (collects-module? mod-path)
  (let loop ([collects (current-library-collection-paths)])
    (match collects
      [`() #f]
      [`(,ch . ,ct)
       (if (string-prefix? (~a mod-path) (~a ch))
           ch
           (loop ct))])))

(define (runtime-module? mod-path)
  (and (string-prefix? (~a mod-path) (~a racketscript-runtime-dir))
       (find-relative-path racketscript-runtime-dir mod-path)))

(define (module-kind mod-path)
  (acond
    [(symbol? mod-path) (list 'primitive mod-path)]
    [(runtime-module? mod-path) (list 'runtime it)]
    [(collects-module? mod-path)
     (list 'collects
           it
           (find-relative-path it mod-path))]
    [(links-module? mod-path)
     (list 'links
           (car it)
           (cadr it)
           (find-relative-path (cadr it) mod-path))]
    [else (list 'general mod-path)]))

(define (converge fn init-val)
  (let loop ([val init-val])
    (let ([new-val (fn val)])
      (if (equal? new-val val)
          new-val
          (loop new-val)))))

;;; ---------------------------------------------------------------------------

(define (sequence-length s)
  (unless (sequence? s) (raise-argument-error 'sequence-length "sequence?" s))
  (cond [(exact-nonnegative-integer? s) s]
        [(list? s) (length s)]
        [(vector? s) (vector-length s)]
        [(flvector? s) (flvector-length s)]
        [(fxvector? s) (fxvector-length s)]
        [(string? s) (string-length s)]
        [(bytes? s) (bytes-length s)]
        [(hash? s) (hash-count s)]
        [else
         (for/fold ([c 0]) ([i (in-values*-sequence s)])
           (add1 c))]))

(define-syntax (for/fold/last stx)
  (syntax-case stx ()
    [(_ ([accum-id bw ... init-expr] ...) ([item bw2 ... is-last? lst] ...) body ...)
     (with-syntax ([(lst-len ...) (generate-temporaries #'(lst ...))]
                   [(lst-i ...) (generate-temporaries #'(lst ...))])
       #'(let ([lst-len (sequence-length lst)] ...)
           (for/fold ([accum-id bw ... init-expr] ...)
                     ([item bw2 ... lst] ...
                      [lst-i (range lst-len)] ...)
             (let ([is-last? (equal? (sub1 lst-len) lst-i)] ...)
               body ...))))]))

(define-syntax (for/last? stx)
  (syntax-case stx ()
    [(_ ([item bw ... is-last? lst] ...) body ...)
     (with-syntax ([(lst-len ...) (generate-temporaries #'(lst ...))]
                   [(lst-i ...) (generate-temporaries #'(lst ...))])
       #'(let ([lst-len (sequence-length lst)] ...)
           (for ([item bw ... lst] ...
                 [lst-i (range lst-len)] ...)
             (let ([is-last? (equal? (sub1 lst-len) lst-i)] ...)
               body ...))))]))
