#lang racketscript/base

;; Handle the the problem when we translate tail calls to
;; to loop and have closures pointing to changed variables.

(define (loop-funcs n)
  (define c n)
  (let loop ([i 0]
             [funcs '()])
    (set! c (add1 c))
    (if (= i n)
      (reverse funcs)
      (loop (add1 i)
            (cons (lambda ()
                    (displayln (list c i)))
                  funcs)))))

(define (invoke-all f*)
  (for-each (lambda (f)
              (f))
            f*))

(invoke-all (loop-funcs 5))

