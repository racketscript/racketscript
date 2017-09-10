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

(displayln
 (with-continuation-mark 'key 'value
   (let ([k2 (string->uninterned-symbol "key")])
   (with-continuation-mark k2 'inner-value
     (list (extract-current-continuation-marks k2)
           (extract-current-continuation-marks 'key))))))

(displayln
 (with-continuation-mark (box 'a) 'value
   (let ([k2 (box 'b)])
   (with-continuation-mark k2 'inner-value
     (list (extract-current-continuation-marks (box 'a))
           (extract-current-continuation-marks (box 'b))
           (extract-current-continuation-marks k2))))))

;; Since RS numbers may not be JS numbers forever.
(displayln
 (with-continuation-mark 123 'value
   (extract-current-continuation-marks 123)))
