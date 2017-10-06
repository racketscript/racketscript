#lang racketscript/base

(require racketscript/interop)

(#js*.console.log (for/js-object ([i (in-range 10)]
                                  [j (in-range 10)])
                    (values (+ i j) (* i j))))
