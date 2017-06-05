#lang racket

(require (for-meta 1 "private/bindings-phase-provide-same.rkt"))

(begin-for-syntax
  (require racket)
  (begin-for-syntax
    (require racket)
    (foo)
    (begin-for-syntax
      (foo)))
  (foo))
