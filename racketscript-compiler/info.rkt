#lang info

(define collection 'multi)
(define version "0.1")

(define deps
  '("base"
    ["racket" "6.4"]
    "typed-racket-lib"
    "typed-racket-more"
    "threading"
    "graph"
    "anaphoric"))

(define build-deps '("base"
                     "typed-racket-lib"
                     "typed-racket-more"
                     "rackunit-lib"))

(define pkg-authors '(vishesh))
(define pkg-desc "Racket to JavaScript compiler")

;; Test configuration

(define test-omit-paths '("racketscript/browser.rkt"
                          "racketscript/compiler/runtime/"))
(define cover-omit-paths '("racketscript/browser.rkt"
                           "racketscript/compiler/runtime/"))
