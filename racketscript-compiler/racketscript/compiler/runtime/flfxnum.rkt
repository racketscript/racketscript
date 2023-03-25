#lang racketscript/boot

(require racketscript/interop
         racketscript/compiler/directive
         "kernel.rkt"
         "lib.rkt"
         (for-syntax syntax/parse))

(define+provide fl*  (#js.Core.attachProcedureArity (if-scheme-numbers #js.Core.Number.Scheme.multiply
                                                                       #js.Core.Number.JS.mul ) 0))
(define+provide fl/  (#js.Core.attachProcedureArity (if-scheme-numbers #js.Core.Number.Scheme.divide
                                                                       #js.Core.Number.JS.div ) 1))
(define+provide fl+  (#js.Core.attachProcedureArity (if-scheme-numbers #js.Core.Number.Scheme.add
                                                                       #js.Core.Number.JS.add ) 0))
(define+provide fl-  (#js.Core.attachProcedureArity (if-scheme-numbers #js.Core.Number.Scheme.subtract
                                                                       #js.Core.Number.JS.sub ) 1))
(define+provide fl<  (#js.Core.attachProcedureArity (if-scheme-numbers #js.Core.Number.Scheme.lessThan
                                                                       #js.Core.Number.JS.lt ) 1))
(define+provide fl>  (#js.Core.attachProcedureArity (if-scheme-numbers #js.Core.Number.Scheme.greaterThan
                                                                       #js.Core.Number.JS.gt ) 1))
(define+provide fl<= (#js.Core.attachProcedureArity (if-scheme-numbers #js.Core.Number.Scheme.lessThanOrEqual
                                                                       #js.Core.Number.JS.lte ) 1))
(define+provide fl>= (#js.Core.attachProcedureArity (if-scheme-numbers #js.Core.Number.Scheme.greaterThanOrEqual
                                                                       #js.Core.Number.JS.gte ) 1))
(define+provide fl=  (#js.Core.attachProcedureArity (if-scheme-numbers #js.Core.Number.Scheme.approxEquals
                                                                       #js.Core.Number.JS.equals ) 1))

(define+provide flabs (if-scheme-numbers #js.Core.Number.Scheme.abs
                                         #js.Math.abs))
(define+provide flmin (if-scheme-numbers min
                                         #js.Math.min))
(define+provide flmax (if-scheme-numbers max
                                         #js.Math.max))
(define+provide flround (if-scheme-numbers round
                                           #js.Math.round))
(define+provide flfloor (if-scheme-numbers floor
                                           #js.Math.floor))
(define+provide flceiling (if-scheme-numbers ceiling
                                             #js.Math.ceil))
(define+provide fltruncate (if-scheme-numbers truncate
                                              #js.Math.trunc))

(define+provide flsin (if-scheme-numbers sin
                                         #js.Math.sin))
(define+provide flcos (if-scheme-numbers cos
                                         #js.Math.cos))
(define+provide fltan (if-scheme-numbers tan
                                         #js.Math.tan))
(define+provide flasin (if-scheme-numbers asin
                                          #js.Math.asin))
(define+provide flacos (if-scheme-numbers acos
                                          #js.Math.acos))
(define+provide flatan (if-scheme-numbers atan
                                          #js.Math.atan))
(define+provide fllog (if-scheme-numbers log
                                         #js.Math.log))
(define+provide flexp (if-scheme-numbers exp
                                         #js.Math.exp))
(define+provide flsqrt (if-scheme-numbers sqrt
                                          #js.Math.sqrt))
(define+provide flexpt (if-scheme-numbers expt
                                          #js.Math.pow))
  
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
(define+provide fxnot #js.Core.bitwiseNot)

(define+provide flvector #js.Array.from) ; just create regular array
(define+provide flvector? #js.Array.isArray)
(define+provide fxvector #js.Array.from) ; just create regular array
(define+provide fxvector? #js.Array.isArray)
