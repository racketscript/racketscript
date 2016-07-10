#lang racket/base

(require "fact.rkt")

(displayln "imported")
(displayln (factorial 5))
(displayln (factorial/tail 6 1))

