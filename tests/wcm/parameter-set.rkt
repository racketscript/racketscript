#lang racket

(define p1 (make-parameter "p1-1"))
(define p2 #f)

(parameterize ([p1 "p1-2"])
  (displayln (list (p1)))
  (define p2* (make-parameter "p2-1"))
  (displayln (list (p1)))
  (p2* "p2-2")
  (displayln (list (p1)))
  (parameterize ([p2* "p2-3"])
    (displayln (list (p1) (p2*)))
    (p2* "p2-4")
    (displayln (list (p1) (p2*)))
    (set! p2 p2*)
    (displayln (list (p1) (p2)))))

(displayln (list (p1) (p2)))
