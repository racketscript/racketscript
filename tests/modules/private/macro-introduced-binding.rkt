#lang racket

(provide invoke-macro)

(define (internal-func)
  (displayln "called internal function"))

(define-syntax (invoke-macro stx)
  (syntax-case stx ()
    [(_) #'(internal-func)]))
