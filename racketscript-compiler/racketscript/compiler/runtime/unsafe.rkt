#lang racketscript/boot

(require racketscript/interop)

(define Core   ($/require "./core.js"))

;;-----------------------------------------------------------------------------
;; Pairs

(define (unsafe-car v) #js.v.hd)
(define (unsafe-cdr v) #js.v.tl)

(provide unsafe-car
         unsafe-cdr)

;;-----------------------------------------------------------------------------
;; Structures

(define (unsafe-struct-ref v k)
  ($ #js.v._fields k))

(provide unsafe-struct-ref)

;;-----------------------------------------------------------------------------
;; Numbers

(define unsafe-fx< #js.Core.Number.lt)
(define unsafe-fx+ #js.Core.Number.add)

(provide unsafe-fx<
         unsafe-fx+)
