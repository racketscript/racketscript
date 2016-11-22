#lang racketscript/base

(define val 0)

(define (foo m)
  (displayln (list m val))
  (set! val (add1 val))
  (list "food" m))

(define (exe a b c d)
  (displayln "exe"))

(exe (let ([n (foo 'a)])
       (foo n))
     (let ([n (foo 'b)])
       (foo n))
     (let ([n (foo 'c)])
       (foo n))
     (let ([n (foo 'd)])
       (foo n)))
