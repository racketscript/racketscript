#lang racket/base

(define pt (make-continuation-prompt-tag 'pt))

(displayln
 (with-continuation-mark 'k1 'v1
   (with-continuation-mark 'k2 'v2
     (list 'ok
           (call-with-continuation-prompt
            (lambda ()
              (abort-current-continuation pt "Hello" "Abort"))
            pt
            (lambda (a b) (list a b)))))))

(define pt-2 (make-continuation-prompt-tag 'pt))

(displayln
 (with-continuation-mark 'k1 'v1
   (call-with-continuation-prompt
    (lambda ()
      (with-continuation-mark 'k2 'v2
        (list 'ok
              (call-with-continuation-prompt
               (lambda ()
                 (abort-current-continuation pt-2 "Hello" "Abort"))
               pt
               (lambda (a b) (list "inner" a b))))))
    pt-2
    (lambda (a b)
      (list "outer" a b)))))
