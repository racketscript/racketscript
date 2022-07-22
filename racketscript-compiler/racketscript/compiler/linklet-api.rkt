#lang racket/base

(require "absyn.rkt"
         "sexp-to-ast.rkt")

(define (compile-linklet sexp)
  (parse-linklet sexp ""))

(define (linklet-import-variables lnk)
  (Linklet-imports lnk))

