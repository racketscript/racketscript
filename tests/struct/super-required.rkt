#lang racket/base

(require "p.rkt")

;; fixes pr #272
;; was getting arity mismatch err bc imported super struct's fields are counted
;; (there are subsequent errors as well if arity check is disabled)
;; (real-world example where this shows up is promises)
(define-struct (p2 p) ())
(displayln (p2 1))
(displayln (p 2))
(displayln (p? (p 2)))
(displayln (p? (make-p 2)))
(displayln (p? (p2 2)))
