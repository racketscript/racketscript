#lang typed/racket/base

(require racket/match
         racket/list
         racket/string
         racket/format)

(provide hash-set-pair*
         fresh-id
         normalize-symbol
         flatten1
         append1
         split-before-last
         for/fold-last
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


(: flatten1 (∀ (A) (-> (Listof (Listof A)) (Listof A))))
(define (flatten1 lst)
  (foldl (inst append A) '() lst))

(: normalize-symbol (-> Symbol Symbol))
(define (normalize-symbol s)
  ;; TODO: handle every weird character in symbol
  ;; Since every identifier is suffixed with fresh symbol
  ;; we don't have to worry about name clashes after this
  ;; naive renaming
  (string->symbol (string-replace (~a s) "-" "_")))

(: fresh-id (-> Symbol Symbol))
(define fresh-id gensym)

(: append1 (∀ (A) (-> (Listof A) A (Listof A))))
(define (append1 lst a)
  (append lst (list a)))

(: split-before-last : (∀ (A) (-> (Listof A) (Values (Listof A) A))))
(define (split-before-last lst)
  (match-define-values (ls (list v)) (split-at-right lst 1))
  (values ls v))


(define-syntax (for/fold-last stx)
  (syntax-case stx ()
    [(_ ([accum-id init-expr] ...) ([item is-last? lst] ...) body ...)
     (with-syntax ([(lst-len ...) (generate-temporaries #'(lst ...))]
                   [(lst-i ...) (generate-temporaries #'(lst ...))])
       #'(let ([lst-len (length lst)] ...)
           (for/fold ([accum-id init-expr] ...)
                     ([item lst] ...
                      [lst-i (range lst-len)] ...)
             (let ([is-last? (equal? (sub1 lst-len) lst-i)] ...)
               body ...))))]))
