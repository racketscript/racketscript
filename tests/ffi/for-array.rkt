#lang racketscript/base

(require racketscript/interop)

(define (sqr x) (* x x))

(#js*.console.log (for/js-array ([i (in-range 10)])
                                (sqr i)))
