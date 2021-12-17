#lang racket/base

(provide add sub)

(require racket/string)
(require racket/list racket/base)

(define (add a b)
  (+ a b))

(define (sub a b)
  (- a b))

;; require protected id, see pr#284
(require "protect-out.rkt")
(displayln (f 10))
