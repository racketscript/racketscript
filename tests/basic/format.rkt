#lang racket/base

(displayln (format "~a" 1))
(displayln (format "~a" "1"))
(displayln (format "~a" #\b))
(displayln (format "~x" 1))
(displayln (format "~x" 50))
(displayln (format "~a is ~x in hex" "15" 15))
