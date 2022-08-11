#lang racket/base

(require "ast.rkt"
         "sexp-to-ast.rkt"
         "transform.rkt"
         "il-analyze.rkt"
         "assembler.rkt")

(provide (all-defined-out))

(define (compile-linklet sexp)
  (absyn-linklet->il (parse-linklet sexp "/home/gamburgm/racketscript/expander.rkt")))

(define (linklet-import-variables lnk)
  (Linklet-imports lnk))

;; TODO temporary, for debugging/checking purposes
(define (everything sexp)
  (define output-str (open-output-string))
  (assemble-linklet
    (insert-arity-checks
      (absyn-linklet->il (parse-linklet sexp "/home/gamburgm/racketscript/expander.rkt")))
    output-str)
  (get-output-string output-str))

