#lang typed/racket/base

(require racket/format
         racket/match
         "config.rkt"
         "util.rkt")

(provide name-in-module
         BASE-ENV)

(: name-in-module (-> Symbol Symbol Symbol))
(define (name-in-module mod name)
  (match mod
    ['core (string->symbol (~a (jsruntime-core-module) "." name))]
    ['kernel (string->symbol (~a (jsruntime-kernel-module) "." name))]
    [_ (error "Invalid module name")]))

(: BASE-ENV (HashTable Symbol Symbol))
(define BASE-ENV (hash))
