#lang racketscript/boot

(require racketscript/interop
         "lib.rkt")

(define Core   ($/require "./core.js"))

;;-----------------------------------------------------------------------------
;; Pairs

(define+provide (unsafe-car v) #js.v.hd)
(define+provide (unsafe-cdr v) #js.v.tl)
(define+provide (unsafe-mcar v) #js.v.hd)
(define+provide (unsafe-mcdr v) #js.v.tl)
(define+provide (unsafe-set-mcar! p v) (#js.p.setCar v))
(define+provide (unsafe-set-mcdr! p v) (#js.p.setCdr v))

;;-----------------------------------------------------------------------------
;; Structures

(define+provide (unsafe-struct-ref v k)
  ($ #js.v._fields k))

;;-----------------------------------------------------------------------------
;; Vector
(define+provide (unsafe-vector-ref v k)
  (#js.v.ref k))

(define+provide (unsafe-vector-set! v k val)
  (#js.v.set k val))

(define+provide (unsafe-vector-length v)
  (#js.v.length))

;;-----------------------------------------------------------------------------
;; Numbers

(define+provide unsafe-fx< #js.Core.Number.lt)
(define+provide unsafe-fx<= #js.Core.Number.lte)
(define+provide unsafe-fx> #js.Core.Number.gt)
(define+provide unsafe-fx>= #js.Core.Number.gte)
(define+provide unsafe-fx= #js.Core.Number.equals)
(define+provide unsafe-fx+ #js.Core.Number.add)
(define+provide unsafe-fx- #js.Core.Number.sub)
(define+provide unsafe-fx* #js.Core.Number.mul)
(define+provide unsafe-fx/ #js.Core.Number.div)
(define+provide (unsafe-fxrshift a b) ($/binop >> a b))
(define+provide (unsafe-fxlshift a b) ($/binop << a b))
(define+provide (unsafe-fxmin a b) (if ($/binop < a b) a b))
(define+provide (unsafe-fxmax a b) (if ($/binop > a b) b a))
