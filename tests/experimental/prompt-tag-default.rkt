#lang racket/base

(define pt (make-continuation-prompt-tag 'pt))

(with-continuation-mark 'k1 'v1
  (call-with-continuation-prompt
   (lambda ()
     (with-continuation-mark 'k2 'v2
       (displayln
        (list 'ok
              (call-with-continuation-prompt
               (lambda ()
                 (with-continuation-mark 'k1 'v3
                   (continuation-mark-set->list (current-continuation-marks) 'k1)))
               pt
               (lambda () (displayln "error")))))))
   (default-continuation-prompt-tag)
   (lambda () (displayln "error"))))
