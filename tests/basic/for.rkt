#lang racket

(define (fn lst)
  (for ([v lst])
    (displayln v)))

(fn (list 1 2 3 4))

;; string sequences
;; needs unsafe-string-length
(for ([c "string"])
  (displayln c))

(for ([c (string->list "string")])
  (displayln c))
