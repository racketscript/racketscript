#lang racketscript/boot

(require racketscript/interop
         "lib.rkt")

(define Paramz ($/require/* "./paramz.js"))

(define+provide parameterization-key #js.Paramz.ParameterizationKey)
(define+provide extend-parameterization #js.Paramz.extendParameterization)
(define+provide exception-handler-key #js.Paramz.ExceptionHandlerKey)
(define+provide (check-for-break) ($/undefined))
