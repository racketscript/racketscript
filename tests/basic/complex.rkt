#lang racket/base

;; zeros
(displayln 0+0i)       ; prints as 0
; (displayln 0+0.0i)   ; prints as 0.0+0.0i
(when (zero? 0+0.0i)
  (displayln 0))
; (displayln 0.0+0i)   ; prints as 0.0
(when (zero? 0.0+0i)
  (displayln 0))
; (displayln 0.0+0.0i) ; prints as 0.0+0.0i
(when (zero? 0.0+0.0i)
  (displayln 0))
