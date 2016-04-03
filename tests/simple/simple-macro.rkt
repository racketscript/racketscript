#lang racket

(provide (all-defined-out))

(define-syntax (if-not stx)
  (syntax-case stx ()
    [(_ test br1 br2)
     #'(if (not test)
         br1
         br2)]))


(if-not (positive? 12)
        (displayln "not positive")
        (displayln "positive"))
