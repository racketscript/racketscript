#lang racketscript/base
(require racketscript/interop)

;; fixes pr#278

(define lib ($/require/rkt "rktlib.rkt"))

(#js.lib.f 10)
