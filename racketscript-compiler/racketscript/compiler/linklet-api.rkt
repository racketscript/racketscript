#lang racket/base

(struct linklet (ast))

(define (compile-linklet sexp)
  (linklet (parse-linklet v)))

