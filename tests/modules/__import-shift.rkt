#lang racket

(require "private/bindings-phase-provide-same.rkt")

(foo)

(begin-for-syntax
  (require racket)
  (begin-for-syntax
    (foo))
  (foo))
