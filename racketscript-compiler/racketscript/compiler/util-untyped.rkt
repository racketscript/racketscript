#lang racket/base

(require "match.rkt")

(provide improper->proper
         *jsident-pattern*
         js-identifier?)

;; Path Path -> Boolean
;; Returns true if path has base as prefix
(define (subpath? base path)
  (define base* (explode-path base))
  (define path* (explode-path path))
  (cond
    [(>= (length base*) (length path*)) #f]
    [else
     (for/and ([b base*]
               [p path*])
       (equal? b p))]))

(define (link-path-elem->string elem)
  (if (symbol? elem)
    elem
    (bytes->string/locale elem)))

(define (get-root-links links-file)
  (define-values (base _f _b) (split-path links-file))
  (let ([specs (read (open-input-file links-file))])
    (for/list ([spec specs]
               #:when (eq? 'root (car spec)))
      (simplify-path
        (apply build-path
              base
              (map link-path-elem->string (cadr spec)))))))

(define (improper->proper l)
  (match l
    [`(,a . ,b) (cons a (improper->proper b))]
    [`() '()]
    [`,_ (cons l '())]))
(module+ test
  (check-equal? (improper->proper '()) '())
  (check-equal? (improper->proper '(1 . 2)) '(1 2))
  (check-equal? (improper->proper '(1 2 3 . 4)) '(1 2 3 4))
  (check-equal? (improper->proper '(1 2 3)) '(1 2 3))
  (check-equal? (improper->proper '(1 2 (3 . 4) . 5)) '(1 2 (3 . 4) 5)))

(define *jsident-start-letter* "\\p{L}|\\p{Nl}|\\$|_")
(define *jsident-rest-letters* (string-append
                                "\\p{L}|\\p{Nl}|\\$|_|\\p{Mn}|\\p{Mc}|\\p{Nd}|\\p{Pc}"
                                "|\u200D|\u200C"))
(define *jsident-pattern* (format "(~a)(~a)*"
                                  *jsident-start-letter*
                                  *jsident-rest-letters*))

;; Symbol -> Boolean
;; Returns true if `sym` is a valid JavaScript identifier
(define (js-identifier? sym)
  (define str (symbol->string sym))
  (regexp-match-exact? (pregexp *jsident-pattern*) str))

(module+ test
  (require rackunit))
