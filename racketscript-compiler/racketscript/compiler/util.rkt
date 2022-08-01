#lang racket/base

(require (for-syntax racket/base)
         "config.rkt"
         "ident.rkt"
         "util-untyped.rkt"
         "set.rkt"
         (only-in racket/string string-prefix?))

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
         (all-from-out "ident.rkt"))

(module+ test (require rackunit))


(define ++ string-append)

(define simple-form-path (compose simplify-path path->complete-path))

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

;; Returns lst with its last element and the last element
(define (split-before-last lst)
  (define-values (ls list-v) (split-at-right lst 1))
  (match list-v
    [`(,v) (values ls v)]))

;; Update given hash `h` with given list of (key, value) pairs.
(define (hash-set-pair* h pairs)
  (let loop ([p* pairs] [h h])
    (if (empty? p*)
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
        (build-path (format "./~a" p-str))))
  (fix-for-down
   (find-relative-path (path-parent (module-output-file base))
                             runtime-fpath)))

;;; Module path renaming ------------------------------------------------------

(define (actual-module-path in-path)
  (cond
    [(path? in-path) in-path]
    [(and (symbol? in-path) (primitive-module? in-path))
     (build-path racketscript-runtime-dir
                 (format "~a~a"
                         (substring (symbol->string in-path) 2)
                         ".rkt"))]
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
                  (format "~a~a"
                          (substring (symbol->string mod) 2)
                          ".rkt"))
      mod))

;; NOTE: returns simplified path, which is required by fns like find-relative-path
(define (module-output-file mod)
  (simple-form-path
    (match (module-kind mod)
      [(list 'primitive mod-path)
      ;; Eg. #%kernel, #%utils ...
       (build-path (output-directory)
                   "runtime"
                   (~a (substring (symbol->string mod-path) 2) ".rkt.js"))]
      [(list 'runtime rel-path)
       (build-path (output-directory) "runtime" (~a rel-path ".js"))]
      [(list 'collects base rel-path)
       (build-path (output-directory) "collects" (~a rel-path ".js"))]
      [(list 'links name root-path rel-path)
       (define output-path
         (build-path (output-directory) "links" name (~a rel-path ".js")))
       ;; because we just created root links directory, but files could be
       ;; deep arbitrarily inside
       (make-directory* (assert (path-only output-path) path?))
       ;; TODO: doesn't handle arbitrary deep files for now
       output-path]
      [(list 'general mod-path)
       (let* ([main (assert (main-source-file) path?)]
               [rel-path (find-relative-path (simple-form-path (path-parent main))
                                             (simple-form-path mod-path))])
         (build-path (output-directory) "modules" (~a rel-path ".js")))])))

(: module->relative-import (-> Path Path))
(define (module->relative-import mod-path)
  ;; ES6 modules imports need "./" prefix for importing relatively
  ;; to current module, rather than relative to main module. Weird :/
  (: fix-for-down (-> Path Path))
  (define (fix-for-down p)
    (define p-str (~a p))
    (if (string-prefix? p-str "..")
        p
        (build-path (~a "./" p-str))))

  (let ([src (current-source-file)])
    (if src
        (fix-for-down
         (cast (find-relative-path (path-parent (module-output-file src))
                                   (module-output-file mod-path))
               Path))
         (error 'module->relative-import "current-source-file is #f"))))

(: collects-module? (-> Path (Option Path)))
(define (collects-module? mod-path)
  (let loop ([collects (current-library-collection-paths)])
    (match collects
      ['() #f]
      [(cons ch ct)
       (if (string-prefix? (~a mod-path) (~a ch))
           ch
           (loop ct))])))

(: runtime-module? (-> Path (Option Path)))
(define (runtime-module? mod-path)
  (and (string-prefix? (~a mod-path) (~a racketscript-runtime-dir))
       (cast (find-relative-path racketscript-runtime-dir mod-path) Path)))

(: module-kind (-> (U Symbol Path)
                   (U (List 'collects  Path Path)
                      (List 'links     String Path Path)
                      (List 'primitive Symbol)
                      (List 'runtime   Path)
                      (List 'general   Path))))
(define (module-kind mod-path)
  (acond
    [(symbol? mod-path) (list 'primitive mod-path)]
    [(runtime-module? mod-path) (list 'runtime it)]
    [(collects-module? mod-path)
     (list 'collects
           it
           (cast (find-relative-path it mod-path) Path))]
    [(links-module? mod-path)
     (list 'links
           (car it)
           (cadr it)
           (cast (find-relative-path (cadr it) mod-path) Path))]
    [else (list 'general mod-path)]))

(: converge (∀ [X] (-> (-> X X) X X)))
(define (converge fn init-val)
  (let loop ([val init-val])
    (let ([new-val (fn val)])
      (if (equal? new-val val)
          new-val
          (loop new-val)))))

;;; ---------------------------------------------------------------------------

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
