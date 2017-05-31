#lang racket

(require "private/macro-introduced-binding.rkt")

(define (execute)
  (invoke-macro))

(define (execute-elsewhere)
  (invoke-macro-with-function-elsewhere))

(execute)
(execute-elsewhere)
