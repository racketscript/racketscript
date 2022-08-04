#lang racket/base

;; basic set implementation 

(provide set set-member? set-add list->set set->list set-map in-set set-intersect)

(struct set-impl (contents) #:transparent)

(define (set . args)
  (set-impl
    (make-immutable-hash (map (λ (e) (cons e #t)) args))))

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

(define (in-set st) (in-list (set->list st)))

(define (set-intersect s . sets)
  (for/fold
      ([s1 (set)])
      ([x (in-set s)]
       #:when (for/and ([s2 (in-list sets)])
                (set-member? s2 x)))
    (set-add s1 x)))
