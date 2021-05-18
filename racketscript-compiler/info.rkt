#lang info

(define collection 'multi)
(define version "0.1")

(define deps
  '("base"
    ["racket" "6.4"]
    "typed-racket-lib"
    "typed-racket-more"
    "threading"
    "graph-lib"
    "anaphoric"))

(define build-deps '("base"
                     "typed-racket-lib"
                     "typed-racket-more"
                     "rackunit-lib"))

(define pkg-authors '(vishesh))
(define pkg-desc "Racket to JavaScript compiler")
(define post-install-collection "")

;; Test configuration

(define test-omit-paths '("racketscript/browser.rkt"
                          "racketscript/compiler/runtime/"))
(define cover-omit-paths '("racketscript/browser.rkt"
                           "racketscript/compiler/runtime/kernel.rkt"
                           "racketscript/compiler/runtime/paramz.rkt"
                           "racketscript/compiler/runtime/unsafe.rkt"))
