#lang racket

(require "private/macro-introduced-binding.rkt")

(define (execute)
  (invoke-macro))

(execute)
