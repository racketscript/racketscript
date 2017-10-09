#lang racketscript/base

(require racketscript/interop)

(for ([v (in-js-array [$/array 1 2 3 4 5 6])])
  (displayln v))

(define arr [$/array 1 2 3 4 5 6])
(for ([v (in-js-array arr)])
  (displayln v))
