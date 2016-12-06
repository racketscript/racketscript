#lang racket

(require "private/module-with-effect.rkt")

(displayln (unbox x))
(set-box! x (add1 (unbox x)))
