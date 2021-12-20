#lang racket/base

;; this is imported by super-required.rkt, see pr #272

;; get "set-p-val! not defined err" if field name is included in this provide
(#%provide (struct p ()))

(define-struct p (val))
(displayln (p 3))
(displayln (p-val (p 3)))
