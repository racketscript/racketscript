#lang info

(define collection 'multi)
(define version "0.1")

(define deps
  '("base"
    ["racket" "6.4"]
    "racketscript-compiler"))

(define build-deps '("rackunit-lib"))

(define pkg-authors '(vishesh))
(define pkg-desc "Extras to play with RacketScript.")
