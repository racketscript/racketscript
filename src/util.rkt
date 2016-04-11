#lang typed/racket/base

(require racket/match
         racket/list
         racket/format
         racket/path
         racket/string
         typed/rackunit
         (for-syntax racket/base)
         "config.rkt")

(require/typed racket/string
  [string-prefix? (-> String String Boolean)])

(provide hash-set-pair*
         fresh-id
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
         module->relative-import
         ++)

(define ++ string-append)

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

(: normalize-symbol (-> Symbol String))
(define (normalize-symbol s)
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
          ("*" . "_star_")
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
(define fresh-id gensym)

(: path-parent (-> Path Path))
;; Because `path-only` return type is `path-for-some-system` and that
;; is not in any way helping
(define (path-parent p)
  (define p* (path-only p))
  (if (path? p*)
      p*
      (error 'path-parent "No parent for ~a" p)))

(: module-path->name (-> (U Path Symbol String) Path)) ;; (-> ModuleName String)
(define (module-path->name mod-name)
  (cond
    [(equal? mod-name '#%kernel) (jsruntime-kernel-module-path)]
    [(string? mod-name) (string->path mod-name)]
    [(path? mod-name) mod-name]
    [else (error 'module-path->name "Don't know how to translate module name '~a'" mod-name)]))

(: main-source-directory (-> Path))
(define (main-source-directory)
  (path-parent (assert (main-source-file) path?)))
    
(: module->relative-import (-> Path Path))
(define (module->relative-import mod-path)
  (define (fix-path-js p)
    (build-path (~a p ".js")))
  (define base (path-parent
                (assert (current-source-file) path?)))
  
  (define (rename-collects m)
    (build-path ".." ;; TODO: Problem is source at arbitrary level
                (find-relative-path (racket-collects-dir) mod-path)))
  
  (cond
    [(collects-module? mod-path) (fix-path-js (rename-collects mod-path))]
    [else (fix-path-js (find-relative-path base mod-path))]))

(: collects-module? (-> (U String Path) Boolean))
(define (collects-module? mod-path)
  (string-prefix? (~a mod-path) (~a (racket-collects-dir))))

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
