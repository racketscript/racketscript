#lang racket

(displayln (if (zero? 10)
             'true
             'false))


(displayln (if 0
             'true
             'false))

(displayln (if ""
             'true
             'false))

(displayln (if 0.0
             'true
             'false))

(displayln (if #f
             'true
             'false))

(displayln (if '()
             'true
             'false))
