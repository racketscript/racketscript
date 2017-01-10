;; Langauge to write RacketScript's #%kernel. Export nothing except what's
;; really needed.

(module boot '#%kernel
  (#%require racket/base
             (for-syntax racket/base)
             (only "interop.rkt"))

  (#%provide #%module-begin
             #%app
             #%top
             #%datum
             require
             provide
             for-syntax
             for-template
             begin-for-syntax
             quote
             if
             lambda
             λ
             case-lambda
             begin
             begin0
             let-values
             letrec-values
             let
             let*
             letrec
             define
             define-values
             define-syntaxes
             define-syntax
             define-syntax-rule
             cond
             when
             unless
             else
             case
             set!
             or
             and
             not
             (for-syntax (all-from racket/base))
             (all-from "interop.rkt"))
  #;end)
