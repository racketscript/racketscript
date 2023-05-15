#lang racket

(provide links-module?
         improper->proper
         *jsident-pattern*
         js-identifier?)

(require (for-syntax syntax/parse)
         setup/dirs
         setup/link)

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

;; Module-Path -> (Maybe (list String Path))
;; If `mod-path` belongs to a module listed in (find-links-file),
;; return a list containing:
;; - the link name,
;; - and path to root of the module
;; e.g., '("racketscript-compiler"
;;         #<path:/home/username/racketscript/racketscript-compiler>)
;; else return false.
(define (links-module? mod-path)
  (define links-file (find-links-file))
  (for*/or ([links-file (current-library-collection-links)]
            #:when links-file
            [link-path (links #:file links-file #:root? #t)])
    (and (subpath? link-path mod-path)
         (let-values ([(base link-name dir?) (split-path link-path)])
           (list (~a link-name) link-path)))))

(define (improper->proper l)
  (match l
    [(cons a b) (cons a (improper->proper b))]
    ['() '()]
    [_ (cons l '())]))
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
