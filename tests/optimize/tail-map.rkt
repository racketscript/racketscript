#lang racket

(define (x-map fn lst)
  (define (loop lst acc)
    (cond
      [(empty? lst) (reverse acc)]
      [else (loop (cdr lst)
                  (cons (fn (car lst)) acc))]))
  (loop lst '()))

(displayln (x-map (lambda (x) (* x x)) '(1 2 3 4 5)))
(displayln (x-map (lambda (x) x) '(1 2 3 4 5)))
