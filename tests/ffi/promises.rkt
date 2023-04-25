#lang racketscript/base
(require "promise-handlers.rkt")

;; see also async.rkt

($> (mkpromise)
    (then handler1)
    (catch errhandle)
    (then handler2))

;; ## prints:
;; Initial
;; handler1:
;;   do that (from catch)
;; handler2:
;;   do this no matter what (from last then)
