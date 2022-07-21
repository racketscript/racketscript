#lang racket/base

(require "absyn.rkt"
         "linklet-expand.rkt")

(define (compile-linklet sexp)
  (parse-linklet v ""))

(define (linklet-import-variables lnk)
  (Linklet-imports lnk))

