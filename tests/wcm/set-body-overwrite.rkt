#lang racket

(define (extract-current-continuation-marks key)
  (continuation-mark-set->list
   (current-continuation-marks)
   key))

(define (main)
  (define result (with-continuation-mark 'key 'outer
                   (if (add1 0)
                     (with-continuation-mark 'key 'inner
                       (extract-current-continuation-marks 'key))
                     #f)))
  result)

(displayln (main))
