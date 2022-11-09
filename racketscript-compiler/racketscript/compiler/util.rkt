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
         actual-module-path
         js-identifier?
         path-parent
         length=?
         string-slice
         converge
         override-module-path
         simple-module-path
         primitive-module?
         primitive-module-path?
         ++
         ~a
         sequence-length
         get-major-version
         (all-from-out "ident.rkt"))

(module+ test (require rackunit))


(define ++ string-append)

(define simple-form-path (compose simplify-path path->complete-path))

(define (~a . args)
  (let* ([fmt-lst (build-list (length args) (λ (_) "~a"))]
         [fmt-string (apply string-append fmt-lst)])
    (apply format fmt-string args)))
    
(define (number->string n)
  (define (digit->char d)
    (cond
      [(eq? d 0) #\0]
      [(eq? d 1) #\1]
      [(eq? d 2) #\2]
      [(eq? d 3) #\3]
      [(eq? d 4) #\4]
      [(eq? d 5) #\5]
      [(eq? d 6) #\6]
      [(eq? d 7) #\7]
      [(eq? d 8) #\8]
      [(eq? d 9) #\9]))

  (let loop ([res '()]
             [curr n])
    (if (zero? curr)
      (list->string (reverse res))
      (loop (cons (digit->char (modulo n 10)))
            (quotient n 10)))))

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

(define (runtime-module? mod-path)
  (and (string-prefix? (~a mod-path) (~a racketscript-runtime-dir))
       (find-relative-path racketscript-runtime-dir mod-path)))

(define (converge fn init-val)
  (let loop ([val init-val])
    (let ([new-val (fn val)])
      (if (equal? new-val val)
          new-val
          (loop new-val)))))

;;; ---------------------------------------------------------------------------

(define (get-major-version str)
  (for/fold ([res 0])
            ([chr (in-string str)]
             #:break (eq? chr #\.))
    (+ (char->number chr) (* 10 res))))

(define (char->number chr)
  (cond
    [(eq? chr #\0) 0]
    [(eq? chr #\1) 1]
    [(eq? chr #\2) 2]
    [(eq? chr #\3) 3]
    [(eq? chr #\4) 4]
    [(eq? chr #\5) 5]
    [(eq? chr #\6) 6]
    [(eq? chr #\7) 7]
    [(eq? chr #\8) 8]
    [(eq? chr #\9) 9]))

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
