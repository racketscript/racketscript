#lang racket/base

;; basic set implementation 

(require (for-syntax racket/base))

(struct set-impl (contents) #:transparent)

(provide set set-member?)

(define-syntax (set stx)
  (syntax-case stx ()
    [(_ e ...) #'(set-impl (make-immutable-hash (list (cons e #t) ...)))]))

(define (set-member? st v) (hash-has-key? (set-impl-contents st) v))
