#lang info

(define collection 'multi)
(define version "0.1")

(define deps
  (list "racket"
        "typed-racket"
        "threading"
        "graph"))

(define build-deps '())

(define pkg-authors '(vishesh))
(define pkg-desc "Racket to JavaScript compiler")

;; Test configuration

(define test-omit-paths '("racketscript/browser.rkt"))
(define cover-omit-paths '("racketscript/browser.rkt"))
