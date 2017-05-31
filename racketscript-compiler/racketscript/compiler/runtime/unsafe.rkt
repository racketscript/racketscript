#lang racketscript/boot

(require racketscript/interop)

(define Core   ($/require "./core.js"))

;;-----------------------------------------------------------------------------
;; Pairs

(define (unsafe-car v) #js.v.hd)
(define (unsafe-cdr v) #js.v.tl)
(define (unsafe-mcar v) #js.v.hd)
(define (unsafe-mcdr v) #js.v.tl)
(define (unsafe-set-mcar! p v) (#js.p.setCar v))
(define (unsafe-set-mcdr! p v) (#js.p.setCdr v))

(provide unsafe-car
         unsafe-cdr
         unsafe-mcar
         unsafe-mcdr
         unsafe-set-mcar!
         unsafe-set-mcdr!
         unsafe-fxrshift
         unsafe-fxlshift)

;;-----------------------------------------------------------------------------
;; Structures

(define (unsafe-struct-ref v k)
  ($ #js.v._fields k))

(provide unsafe-struct-ref)

;;-----------------------------------------------------------------------------
;; Vector
(define (unsafe-vector-ref v k)
  (#js.v.ref k))

(define (unsafe-vector-set! v k val)
  (#js.v.set k val))

(define (unsafe-vector-length v)
  (#js.v.length))

(provide unsafe-vector-ref
         unsafe-vector-set!
         unsafe-vector-length)

;;-----------------------------------------------------------------------------
;; Numbers

(define unsafe-fx< #js.Core.Number.lt)
(define unsafe-fx<= #js.Core.Number.lte)
(define unsafe-fx> #js.Core.Number.gt)
(define unsafe-fx>= #js.Core.Number.gte)
(define unsafe-fx= #js.Core.Number.equals)
(define unsafe-fx+ #js.Core.Number.add)
(define unsafe-fx- #js.Core.Number.sub)
(define unsafe-fx* #js.Core.Number.mul)
(define unsafe-fx/ #js.Core.Number.div)
(define (unsafe-fxrshift a b) ($/binop >> a b))
(define (unsafe-fxlshift a b) ($/binop << a b))
(define (unsafe-fxmin a b) (if ($/binop < a b) a b))
(define (unsafe-fxmax a b) (if ($/binop > a b) b a))

(provide unsafe-fx<
         unsafe-fx>
         unsafe-fx<=
         unsafe-fx>=
         unsafe-fx=
         unsafe-fxmax
         unsafe-fxmin
         unsafe-fx+
         unsafe-fx-
         unsafe-fx*
         unsafe-fx/)
