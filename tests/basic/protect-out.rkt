#lang racket/base
(define (f x) x)
(define-for-syntax (f1 x) x)
(provide (protect-out f (for-meta 1 f1)))
