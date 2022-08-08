#lang racket/base

(require "ast.rkt"
         "sexp-to-ast.rkt"
         "transform.rkt")

(provide (all-defined-out))

(define (compile-linklet sexp)
  (absyn-linklet->il (parse-linklet sexp "/home/gamburgm/racketscript/expander.rkt")))

(define (linklet-import-variables lnk)
  (Linklet-imports lnk))
