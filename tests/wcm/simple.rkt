#lang racket

(define (extract-current-continuation-marks key)
  (continuation-mark-set->list
   (current-continuation-marks)
   key))

(displayln
 (with-continuation-mark 'key 'mark
   (extract-current-continuation-marks 'key)))

(displayln
 (with-continuation-mark 'key1 'mark1
   (with-continuation-mark 'key2 'mark2
     (list
      (extract-current-continuation-marks 'key1)
      (extract-current-continuation-marks 'key2)))))

(displayln
 (with-continuation-mark 'key 'mark1
   (with-continuation-mark 'key 'mark2 ; replaces previous mark
     (extract-current-continuation-marks 'key))))

(displayln
 (with-continuation-mark 'key 'mark1
   (list ; continuation extended to evaluate the argument
    (with-continuation-mark 'key 'mark2
      (extract-current-continuation-marks 'key)))))
