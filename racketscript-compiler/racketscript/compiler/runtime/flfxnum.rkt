#lang racketscript/boot

(require racketscript/interop "lib.rkt" (for-syntax syntax/parse))

(define+provide fl*  (#js.Core.attachProcedureArity #js.Core.Number.mul 0))
(define+provide fl/  (#js.Core.attachProcedureArity #js.Core.Number.div 1))
(define+provide fl+  (#js.Core.attachProcedureArity #js.Core.Number.add 0))
(define+provide fl-  (#js.Core.attachProcedureArity #js.Core.Number.sub 1))
(define+provide fl<  (#js.Core.attachProcedureArity #js.Core.Number.lt 1))
(define+provide fl>  (#js.Core.attachProcedureArity #js.Core.Number.gt 1))
(define+provide fl<= (#js.Core.attachProcedureArity #js.Core.Number.lte 1))
(define+provide fl>= (#js.Core.attachProcedureArity #js.Core.Number.gte 1))
(define+provide fl=  (#js.Core.attachProcedureArity #js.Core.Number.equals 1))

(define+provide flabs #js.Math.abs)
(define+provide flmin #js.Math.min)
(define+provide flmax #js.Math.max)
(define+provide flround #js.Math.round)
(define+provide flfloor #js.Math.floor)
(define+provide flceiling #js.Math.ceil)
(define+provide fltruncate #js.Math.trunc)

(define+provide flsin #js.Math.sin)
(define+provide flcos #js.Math.cos)
(define+provide fltan #js.Math.tan)
(define+provide flasin #js.Math.asin)
(define+provide flacos #js.Math.acos)
(define+provide flatan #js.Math.atan)
(define+provide fllog #js.Math.log)
(define+provide flexp #js.Math.exp)
(define+provide flsqrt #js.Math.sqrt)
(define+provide flexpt #js.Math.pow)
  
(define-binop bitwise-or \|)

(define-syntax (define-fx-binop+provide stx)
  (syntax-parse stx
    [(_ opname:id op:id)
     #'(begin         
         (define+provide (opname a b)
           (bitwise-or (binop op a b) 0)))]))

(define-fx-binop+provide fx+         +)
(define-fx-binop+provide fx-         -)
(define-fx-binop+provide fx*         *)
(define-fx-binop+provide fxquotient  /)
(define-fx-binop+provide fxremainder %)

(define+provide (fxmodulo a b)
  (define remainder (binop % a b))
  (#js.Math.floor (if (binop >= remainder 0)
                      remainder
                      (binop + remainder b))))

(define+provide (fxabs a)
  (#js.Math.abs a))


(define+provide (fx= a b)
  (binop === a b))
(define+provide (fx< a b)
  (binop < a b))
(define+provide (fx<= a b)
  (binop <= a b))
(define+provide (fx> a b)
  (binop > a b))
(define+provide (fx>= a b)
  (binop >= a b))
(define+provide (fxmin a b)
  (if ($/binop < a b) a b))
(define+provide (fxmax a b)
  (if ($/binop > a b) b a))


(define-fx-binop+provide fxrshift  >>)
(define-fx-binop+provide fxlshift  <<)
(define-fx-binop+provide fxand     &&)
(define-fx-binop+provide fxior     \|\|)
(define-fx-binop+provide fxxor     ^)
(define+provide fxnot                     #js.Core.bitwiseNot)

