#lang racket/base

(define pt (make-continuation-prompt-tag 'pt))

(displayln
 (with-continuation-mark 'k1 'v1
   (with-continuation-mark 'k2 'v2
     (list 'ok
           (call-with-continuation-prompt
            (lambda (a b)
              (list a b))
            pt
            (lambda () (displayln "error"))
            "Hello." "World.")))))
