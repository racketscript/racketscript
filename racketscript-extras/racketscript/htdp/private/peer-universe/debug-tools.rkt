#lang racketscript/base

(require "encode-decode.rkt")

(provide console-log-rkt-list
         test-encoding)

(define (console-log-rkt-list l)
  (if (list? l) (#js*.console.log (foldl (lambda (curr res)
                                    (#js.res.push curr)
                                    res)
                                  ($/array) l))
                (#js*.console.log l)))

(define (test-encoding val)
  (define result (decode-data (encode-data val)))
  (#js*.console.log val)
  (#js*.console.log result)
  (#js*.console.log (js-string (format "val == result? : ~a" (equal? val result))))
  (void))