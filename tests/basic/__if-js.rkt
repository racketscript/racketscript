#lang racketscript/base

;; Skipped from default because Racket can't run these.

(require racketscript/interop)


(if $/undefined
  (displayln "not undefined")
  (displayln "undefined"))

(if $/null
  (displayln "not null")
  (displayln "null"))

(if ($/array)
  (displayln "array")
  (displayln "not array"))

(if ($/obj)
  (displayln "object")
  (displayln "not object"))
