#lang racketscript/boot

(require racketscript/interop)

(define Paramz ($/require "./paramz.js"))

(provide parameterization-key
         extend-parameterization)

(define parameterization-key #js.Paramz.ParameterizationKey)
(define extend-parameterization #js.Paramz.extendParameterization)
