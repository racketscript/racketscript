#lang racket/base

;; basic set implementation 

(require (for-syntax racket/base))

(provide set set-member? set-add list->set set->list set-map)

(struct set-impl (contents) #:transparent)

(define-syntax (set stx)
  (syntax-case stx ()
    [(_ e ...) #'(set-impl (make-immutable-hash (list (cons e #t) ...)))]))

(define (set-member? st v) (hash-has-key? (set-impl-contents st) v))

(define (set-add st v)
  (set-impl (hash-set (set-impl-contents st) v #t)))

(define (set-map st proc)
  (set-impl
    (hash-map (set-impl-contents st)
              (λ (k _) (proc k)))))

(define (list->set lst)
  (for/fold ([st (set)])
            ([elem lst])
    (set-add st elem)))

(define (set->list st) (hash-keys (set-impl-contents st)))
