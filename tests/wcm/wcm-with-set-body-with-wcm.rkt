#lang racket

(define (extract-current-continuation-marks key)
  (continuation-mark-set->list
   (current-continuation-marks)
   key))

(define (main)
  (define result #f)
  (with-continuation-mark 'key 'mark-main
    (set! result (with-continuation-mark 'key 'mark-set
                   (extract-current-continuation-marks 'key))))
    result)

(displayln (main))
