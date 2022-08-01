#lang racket/base

(require "config.rkt"
         "util.rkt"
         "match.rkt")

(provide name-in-module
         *quoted-binding-ident-name*)

(define *quoted-binding-ident-name* '__rjs_quoted__)

(define (name-in-module mod name)
  (match mod
    [`core (string->symbol (~a (jsruntime-core-module) "." name))]
    [`,_ (error "Invalid module name")]))
