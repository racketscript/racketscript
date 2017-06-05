#lang typed/racket/base

(require racket/format
         racket/match
         "config.rkt"
         "util.rkt")

(provide name-in-module
         *quoted-binding-ident-name*)

(define *quoted-binding-ident-name* '__rjs_quoted__)

(: name-in-module (-> Symbol Symbol Symbol))
(define (name-in-module mod name)
  (match mod
    ['core (string->symbol (~a (jsruntime-core-module) "." name))]
    [_ (error "Invalid module name")]))
