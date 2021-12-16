#lang racket/base
;; this is imported by super-required.rkt, see pr #272
(#%provide (struct p ()))
(define-struct p (val))
