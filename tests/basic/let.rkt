#lang racket

(define (foobar a b)
  (let ([c (+ a a)])
    (+ c (let ([a 2])
           (* a b (let ([b 10])
                    b))))))

(displayln (equal? (foobar 5 7) 150))
