#lang racket/base

(require "ast.rkt"
         "sexp-to-ast.rkt")

(provide (all-defined-out))

(define (compile-linklet sexp)
  (parse-linklet sexp ""))

(define (linklet-import-variables lnk)
  (Linklet-imports lnk))

