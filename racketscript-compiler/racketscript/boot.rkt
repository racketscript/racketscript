;; Langauge to write RacketScript's #%kernel. Export nothing except what's
;; really needed.

(module boot '#%kernel
  (#%require (only racket/base
                   require
                   provide
                   let
                   let*
                   letrec
                   define
                   define-syntax
                   cond
                   case
                   or
                   and
                   make-rename-transformer)
             (only "interop.rkt"))

  (#%provide #%module-begin
             #%app
             #%top
             #%datum
             require
             provide
             quote
             if
             lambda
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
             cond
             case
             set!
             or
             and
             make-rename-transformer
             (all-from "interop.rkt"))
  #;end)
