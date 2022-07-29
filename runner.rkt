#lang racket

(require "racketscript-compiler/racketscript/compiler/linklet-api.rkt")

(let ([lnk-sexp (read (open-input-file "expander.rktl"))])
  (compile-linklet lnk-sexp))

