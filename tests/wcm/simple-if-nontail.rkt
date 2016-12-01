#lang racket

(define (extract-current-continuation-marks key)
  (continuation-mark-set->list
   (current-continuation-marks)
   key))

(define (main)
  (with-continuation-mark 'key 'mark-main
    (if (add1 0)
      (let ([r (with-continuation-mark 'key 'mark-if
                 (extract-current-continuation-marks 'key))])
        r)
        #f)))

(displayln (main))
