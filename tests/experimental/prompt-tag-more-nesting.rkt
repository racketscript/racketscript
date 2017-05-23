#lang racket/base

(define pt (make-continuation-prompt-tag 'pt))
(define pt-2 (make-continuation-prompt-tag 'pt-2))

(displayln
 (with-continuation-mark 'k1 'v1
   (with-continuation-mark 'k2 'v2
     (call-with-continuation-prompt
      (lambda ()
        (with-continuation-mark 'k1 'v3
          (with-continuation-mark 'k2 'v4
            (call-with-continuation-prompt
             (lambda ()
               (with-continuation-mark 'k1 'v5
                 (continuation-mark-set->list (current-continuation-marks pt-2) 'k1)))
             pt
             (lambda () "error")))))
      pt-2
      (lambda () "error")))))

(displayln
 (with-continuation-mark 'k1 'v1
   (with-continuation-mark 'k2 'v2
     (call-with-continuation-prompt
      (lambda ()
        (with-continuation-mark 'k1 'v3
          (with-continuation-mark 'k2 'v4
            (call-with-continuation-prompt
             (lambda ()
               (with-continuation-mark 'k1 'v5
                 (continuation-mark-set->list (current-continuation-marks pt-2) 'k1 pt)))
             pt
             (lambda () "error")))))
      pt-2
      (lambda () "error")))))

(displayln
 (with-continuation-mark 'k1 'v1
   (with-continuation-mark 'k2 'v2
     (call-with-continuation-prompt
      (lambda ()
        (with-continuation-mark 'k1 'v3
          (with-continuation-mark 'k2 'v4
            (call-with-continuation-prompt
             (lambda ()
               (with-continuation-mark 'k1 'v5
                 (continuation-mark-set->list (current-continuation-marks) 'k1 pt)))
             pt
             (lambda () "error")))))
      pt-2
      (lambda () "error")))))

(displayln
 (with-continuation-mark 'k1 'v1
   (with-continuation-mark 'k2 'v2
     (call-with-continuation-prompt
      (lambda ()
        (with-continuation-mark 'k1 'v3
          (with-continuation-mark 'k2 'v4
            (call-with-continuation-prompt
             (lambda ()
               (with-continuation-mark 'k1 'v5
                 (continuation-mark-set->list (current-continuation-marks) 'k1 pt-2)))
             pt
             (lambda () "error")))))
      pt-2
      (lambda () "error")))))

(displayln
 (with-continuation-mark 'k1 'v1
   (with-continuation-mark 'k2 'v2
     (call-with-continuation-prompt
      (lambda ()
        (with-continuation-mark 'k1 'v3
          (with-continuation-mark 'k2 'v4
            (call-with-continuation-prompt
             (lambda ()
               (with-continuation-mark 'k1 'v5
                 (continuation-mark-set->list (current-continuation-marks) 'k1)))
             pt
             (lambda () "error")))))
      pt-2
      (lambda () "error")))))
