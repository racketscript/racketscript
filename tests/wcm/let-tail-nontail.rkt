#lang racket

(define (extract-current-continuation-marks key)
  (continuation-mark-set->list
   (current-continuation-marks)
   key))

(define (main)
  (define result-a #f)
  (define result-b #f)
  (with-continuation-mark 'key 'mark-main
    (let ([x (add1 0)])
      (with-continuation-mark 'key 'mark-let
        (set! result-a (extract-current-continuation-marks 'key)))
      (with-continuation-mark 'key 'mark-set
        (set! result-b (extract-current-continuation-marks 'key)))))
  (list result-a result-b))

(displayln (main))
