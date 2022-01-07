#lang racketscript/base
(require "promise-handlers.rkt"
         racketscript/interop)

;; see also promises.rkt

;; ## prints (everything after err is aborted)
;; Initial
;; handler1:
;;   do that (from catch)

;; using async/await
($/define/async (go)
  (define result ($/await (mkpromise)))
  (define res2 ($/await (handler1 result)))
  (define res3 ($/await (handler2 result))) ;; skipped
  (#js*.console.log #js"final result:") ;; skipped
  (#js*.console.log res3))

($> (go)
    (catch errhandle))
  
