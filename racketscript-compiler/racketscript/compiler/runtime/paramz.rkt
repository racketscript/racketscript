#lang racketscript/boot

(require racketscript/interop
         "lib.rkt")

(define Paramz ($/require/* "./paramz.js"))

(define+provide parameterization-key #js.Paramz.ParameterizationKey)
(define+provide break-enabled-key #js.Paramz.BreakEnabledKey)
(define+provide cache-configuration #js.Paramz.BreakEnabledKey)
(define+provide extend-parameterization #js.Paramz.extendParameterization)
(define+provide exception-handler-key #js.Paramz.ExceptionHandlerKey)
(define+provide (check-for-break) ($/undefined))
(define+provide (reparameterize v) v)
