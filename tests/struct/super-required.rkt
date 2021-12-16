#lang racket/base

(require "p.rkt")

;; fixes pr #272
;; was getting arity mismatch err bc imported super struct's fields are counted
;; (there are subsequent errors as well if arity check is disabled)
;; (real-world example where this shows up is promises)
(define-struct (p2 p) ())
(p2 1) 
