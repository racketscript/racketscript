#lang racket

(require "simple-provide.rkt")
(provide invoke-macro
         invoke-macro-with-function-elsewhere)

(define (internal-func)
  (displayln "called internal function"))

(define-syntax (invoke-macro stx)
  (syntax-case stx ()
    [(_) #'(internal-func)]))

(define-syntax (invoke-macro-with-function-elsewhere stx)
  (syntax-case stx ()
    [(_) #'(begin (say-hello "there!")
                  (say-ahoy "World"))]))
