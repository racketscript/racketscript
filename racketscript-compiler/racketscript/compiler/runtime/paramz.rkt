#lang racketscript/boot

(require racketscript/interop)

(define Paramz ($/require "./paramz.js"))

(provide parameterization-key
         extend-parameterization
         exception-handler-key
         check-for-break)

(define parameterization-key #js.Paramz.ParameterizationKey)
(define extend-parameterization #js.Paramz.extendParameterization)

(define exception-handler-key #js.Paramz.ExceptionHandlerKey)
(define (check-for-break) ($/undefined))
