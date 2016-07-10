#lang racket

(define foobar
  (lambda (a b [c "foo"])
    (displayln (list a b c))))

(foobar 1 2)
