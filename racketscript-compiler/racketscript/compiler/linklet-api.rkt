#lang racket/base

(require "ast.rkt"
         "sexp-to-ast.rkt"
         "transform.rkt"
         "il-analyze.rkt"
         "assembler.rkt")

(provide (all-defined-out))

(define (compile-linklet sexp)
  (absyn-linklet->il (parse-linklet sexp "/home/gamburgm/racketscript/racketscript.rkt")))

(define (linklet-import-variables lnk)
  (Linklet-imports lnk))

;; TODO temporary, for debugging/checking purposes
(define (everything)
  (define sexp (read (open-input-file "../../../racketscript.rktl")))

  (call-with-output-file
    "../../../js-build/modules/racketscript.js"
    (λ (out)
      (assemble-linklet
        (insert-arity-checks
          (absyn-linklet->il (parse-linklet sexp "/home/gamburgm/racketscript/racketscript.rkt")))
        out))
    #:exists 'replace))
