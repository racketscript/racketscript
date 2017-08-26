#lang racket/base

(define (foo a b . c)
  (list a b c))

(define (bar . c)
  (list c))

(define (baz a b c)
  (list a b c))

; TODO: These tests fail because JS outputs "kernel:arity-at-least"
;   while Racket outputs "arity-at-least".
; (displayln (procedure-arity foo))
; (displayln (procedure-arity bar))
; (displayln (procedure-arity baz))
