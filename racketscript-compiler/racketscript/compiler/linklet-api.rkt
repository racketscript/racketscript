#lang racket/base

(require "absyn.rkt"
         "linklet-expand.rkt")

(provide (all-defined-out))

(define (compile-linklet sexp)
  (parse-linklet sexp ""))

(define (linklet-import-variables lnk)
  (Linklet-imports lnk))

