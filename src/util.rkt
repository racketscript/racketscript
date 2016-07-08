#lang typed/racket/base

(require racket/match
         racket/list
         racket/format
         racket/path
         racket/string
         racket/file
         typed/rackunit
         (for-syntax racket/base)
         "config.rkt"
         "util-untyped.rkt")

(require/typed racket/string
  [string-prefix? (-> String String Boolean)])

(require/typed "util-untyped.rkt"
  [improper->proper (-> (Pairof Any Any) (Listof Any))]
  [links-module? (-> Path-String
                     (Option (Pairof String (Pairof Path-String '()))))])

(provide hash-set-pair*
         improper->proper
         fresh-id
         fresh-id-counter
         normalize-symbol
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
         jsruntime-import-path
         path-parent
         length=?
         ++)

(: fresh-id-counter (Parameter Nonnegative-Integer))
(define fresh-id-counter (make-parameter 0))

(define ++ string-append)

(: length=? (-> (Listof Any) Natural Boolean))
(define (length=? lst n)
  (equal? (length lst) n))

(: hash-set-pair* (∀ (A B) (-> (HashTable A B) (Listof (Pairof A B)) (HashTable A B))))
(define (hash-set-pair* h pairs)
  (let loop ([p* pairs] [h h])
    (if (empty? p*)
        h
        (let* ([p (car p*)]
               [k (car p)]
               [v (cdr p)])
          (loop (cdr p*) (hash-set h k v))))))

(: reverse-pair (∀ (A B) (-> (Pairof A B) (Pairof B A))))
(define (reverse-pair p)
  (cons (cdr p) (car p)))

(: normalize-symbol (->* (Symbol) ((Listof String)) String))
(define (normalize-symbol s [ignores '()])
  ;; TODO: handle every weird character in symbol
  ;; Since every identifier is suffixed with fresh symbol
  ;; we don't have to worry about name clashes after this
  ;; naive renaming
  (: should-rename? (-> String Boolean))
  (define (should-rename? s)
    (not (or
          (string-prefix? s (jsruntime-core-module))
          (string-prefix? s (jsruntime-kernel-module)))))
  (: char-map (HashTable String String))
  (define char-map
    #hash(("-" . "_")
          ("?" . "_p")
          ("+" . "_plus_")
          ("'" . "_prime_")
          ("*" . "_times_")
          ("/" . "_by_")
          ("=" . "_eq_")
          ("<" . "_lt_")
          (">" . "_gt_")
          ("!" . "_bang_")
          ("." . "_dot_")
          ("&" . "_and_")))
  (match (symbol->string s)
    [str #:when (should-rename? str)
         (: char-list (Listof Char))
         (define char-list (string->list str))
         (string-join
          (map (λ ([ch : Char])
                 (define sch (string ch))
                 (cond
                   [(member (string ch) ignores) sch]
                   [(or (char-numeric? ch) (char-alphabetic? ch))
                    sch]
                   [(hash-has-key? char-map sch)
                    (hash-ref char-map sch)]
                   [else "_"]))
               char-list)
          "")]
    [str str]))

(: flatten1 (∀ (A) (-> (Listof (Listof A)) (Listof A))))
(define (flatten1 lst)
  (foldl (inst append A) '() lst))

(: fresh-id (-> Symbol Symbol))
(define fresh-id
  (if (test-environment?)
      gensym
      (λ (id)
        (fresh-id-counter (add1 (fresh-id-counter)))
        (string->symbol (~a id (fresh-id-counter))))))

(: path-parent (-> Path Path))
;; Because `path-only` return type is `path-for-some-system` and that
;; is not in any way helping
(define (path-parent p)
  (define p* (path-only p))
  (if (path? p*)
      p*
      (error 'path-parent "No parent for ~a" p)))

(: module-path->name (-> (U Path Symbol) Path)) ;; (-> ModuleName String)
(define (module-path->name mod-name)
  (cond
    [(equal? mod-name '#%kernel) (jsruntime-kernel-module-path)]
    [(path? mod-name) mod-name]
    [else (error 'module-path->name "Don't know how to translate module name '~a'" mod-name)]))

(: main-source-directory (-> Path))
(define (main-source-directory)
  (path-parent (assert (main-source-file) path?)))

(: jsruntime-import-path (-> Path Path Path))
(define (jsruntime-import-path base runtime-fpath)
  ;; TODO: Make runtime, modules, and everything united!
  (cast (find-relative-path (path-parent (module-output-file base))
                            runtime-fpath)
        Path))

(: module-output-file (-> Path Path))
(define (module-output-file mod)
  (define collects? (collects-module? mod))
  (define links? (links-module? mod))
  (cond
    [collects?
     (let ([rel-collects (find-relative-path (racket-collects-dir) mod)])
       (path->complete-path
        (build-path (output-directory) "collects" (~a rel-collects ".js"))))]
    [links?
     (match-define (list name root-path) links?)
     (define rel-path (find-relative-path root-path mod))
     (define output-path
       (build-path (output-directory) "links" name (~a rel-path ".js")))
     ;; because we just created root links directory, but files could be
     ;; deep arbitrarily inside
     (make-directory* (assert (path-only output-path) path?))
     ;; TODO: doesn't handle arbitrary deep files for now
     (path->complete-path output-path)]
    [else
     (let* ([main (assert (main-source-file) path?)]
            [rel-path (find-relative-path (path-parent main) mod)])
       (path->complete-path
        (build-path (output-directory) "modules" (~a rel-path ".js"))))]))

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
  ;; FIX: Later when collects is supports, don't use kernel instead
  (let ([src (assert (current-source-file) path?)]
        [collects? (collects-module? mod-path)]
        [links-module? (links-module? mod-path)])
    (fix-for-down
     (cast (find-relative-path (path-parent (module-output-file src))
                               (cond
                                 [collects? (jsruntime-kernel-module-path)]
                                 [else (module-output-file mod-path)]))
           Path))))

(: collects-module? (-> (U String Path) (Option Path)))
(define (collects-module? mod-path)
  (let loop ([collects (current-library-collection-paths)])
    (match collects
      ['() #f]
      [(cons ch ct)
       (if (string-prefix? (~a mod-path) (~a ch))
           ch
           (loop ct))])))

(: append1 (∀ (A) (-> (Listof A) A (Listof A))))
(define (append1 lst a)
  (append lst (list a)))

(: split-before-last : (∀ (A) (-> (Listof A) (Values (Listof A) A))))
(define (split-before-last lst)
  (match-define-values (ls (list v)) (split-at-right lst 1))
  (values ls v))

(: assocs->hash-list : (∀ (A B) (-> (Listof (Pairof A B)) (HashTable A (Listof B)))))
(define (assocs->hash-list assocs)
  (: empty-hash (HashTable A (Listof B)))
  (define empty-hash (hash))
  (foldl (λ ([a* : (Pairof A B)] [result : (HashTable A (Listof B))])
           (let ([key (car a*)]
                 [val (cdr a*)])
             (hash-update result key
                          (λ ([v : (Listof B)]) (cons val v))
                          (λ () '()))))
         empty-hash
         assocs))
(module+ test
  (check-equal? (assocs->hash-list '((a . b) (b . c) (c . d) (a . e) (c . f) (a . g)))
                (hash 'a '(g e b)
                      'b '(c)
                      'c '(f d))))

(define-syntax (for/fold/last stx)
  (syntax-case stx ()
    [(_ ([accum-id bw ... init-expr] ...) ([item bw2 ... is-last? lst] ...) body ...)
     (with-syntax ([(lst-len ...) (generate-temporaries #'(lst ...))]
                   [(lst-i ...) (generate-temporaries #'(lst ...))])
       #'(let ([lst-len (length lst)] ...)
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
       #'(let ([lst-len (length lst)] ...)
           (for ([item bw ... lst] ...
                 [lst-i (range lst-len)] ...)
             (let ([is-last? (equal? (sub1 lst-len) lst-i)] ...)
               body ...))))]))
