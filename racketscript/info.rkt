#lang info

(define collection 'multi)

(define deps
  '("base"
    "racketscript-compiler"
    "racketscript-extras"
    "racketscript-doc"))
(define build-deps
  '())

(define implies
  '("racketscript-compiler"
    "racketscript-extras"
    "racketscript-doc"))

