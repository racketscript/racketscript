#lang info

(define collection 'multi)
(define version "0.1")

(define deps
  (list "racket"
        "typed-racket"
        "threading"
        "graph"))

(define build-deps '())

(define racket-launcher-name (list "rapture"))
(define racket-launcher-libraries (list "./rapture/compiler/main.rkt"))

(define pkg-authors '(vishesh))
(define pkg-desc "Racket to JavaScript compiler")
