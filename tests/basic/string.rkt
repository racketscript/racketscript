#lang racket

(define str "Hello World")
(define num 42)

(displayln str)
(displayln num)

(displayln (string-ref str 5))

(displayln (make-string 5 #\c))

(displayln (list->string '(#\a #\b #\c)))

(displayln (substring str 3))
(displayln (substring str 3 7))
