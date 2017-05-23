#lang racket/base

(define pt (make-continuation-prompt-tag 'pt))

(with-continuation-mark 'k1 'v1
  (with-continuation-mark 'k2 'v2
    (displayln
     (list 'ok
           (call-with-continuation-prompt
            (lambda ()
              (with-continuation-mark 'k1 'v3
                (continuation-mark-set->list (current-continuation-marks) 'k1 pt)))
            pt
            (lambda () (displayln "error")))))))

(with-continuation-mark 'k1 'v1
  (with-continuation-mark 'k2 'v2
    (displayln
     (list 'ok
           (call-with-continuation-prompt
            (lambda ()
              (with-continuation-mark 'k1 'v3
                (continuation-mark-set->list (current-continuation-marks pt) 'k1 pt)))
            pt
            (lambda () (displayln "error")))))))

(with-continuation-mark 'k1 'v1
  (with-continuation-mark 'k2 'v2
    (displayln
     (list 'ok
           (call-with-continuation-prompt
            (lambda ()
              (with-continuation-mark 'k1 'v3
                (continuation-mark-set->list (current-continuation-marks pt) 'k1)))
            pt
            (lambda () (displayln "error")))))))
