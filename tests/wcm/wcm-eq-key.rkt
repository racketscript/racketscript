#lang racket

(struct posn (x y) #:transparent)

(define (extract-current-continuation-marks key)
  (continuation-mark-set->list
   (current-continuation-marks)
   key))

(displayln
 (with-continuation-mark (posn 1 2) 'value
   (extract-current-continuation-marks (posn 1 2))))

(displayln
 (let ([p (posn 1 2)])
   (with-continuation-mark p 'value
     (extract-current-continuation-marks p))))

(displayln
 (with-continuation-mark '(1 2) 'value
   (extract-current-continuation-marks '(1 2))))

(displayln
 (with-continuation-mark 'key 'value
   (extract-current-continuation-marks
    (string->uninterned-symbol "key"))))
