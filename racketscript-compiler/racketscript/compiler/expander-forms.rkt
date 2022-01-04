#lang racket/base

(require racket/set)

(provide EXPANDER-FORMS)

(define EXPANDER-FORMS
  (set 'unsafe-make-place-local
       'unsafe-root-continuation-prompt-tag))

