#lang racketscript/base
(provide (all-defined-out))

;; see also promises.rkt and async.rkt

(define (mkpromise)
  ($/new
   (#js*.Promise
    (lambda (resolve reject)
      (#js*.console.log #js"Initial")
      (resolve null)))))
(define (handler1 res)
  (#js*.console.log #js"handler 1")
  ($/throw ($/new (#js*.Error #js"Something failed")))
  (#js*.console.log #js"  skipped thing")) ; gets skipped

(define (handler2 res)
  (#js*.console.log #js"handler 2")
  (#js*.console.log #js"  do this no matter what")
  #js"final res")

(define (errhandle err)
  (#js*.console.log #js"  do that"))
