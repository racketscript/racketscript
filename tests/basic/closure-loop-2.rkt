#lang racketscript/base

;; Handle the the problem when we translate tail calls to
;; to loop and have closures pointing to changed variables.

(define (loop-funcs n)
  (define c n)
  (define f*
    (let loop ([i 0]
               [funcs '()])
      (if (= i n)
        (reverse funcs)
        (loop (add1 i)
              (cons (lambda ()
                      (displayln (list c i)))
                    funcs)))))
  (set! c "changed")
  f*)

(define (invoke-all f*)
  (for-each (lambda (f)
              (f))
            f*))

(invoke-all (loop-funcs 5))

