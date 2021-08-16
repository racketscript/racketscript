#lang racket/base

(define vec (vector 0 1 2 3))

(displayln vec)
(displayln (vector-ref vec 0))
(displayln (vector-ref vec 1))
(displayln (vector-ref vec 2))
(displayln (vector-ref vec 3))
(vector-set! vec 0 3)
(vector-set! vec 1 2)
(vector-set! vec 2 1)
(vector-set! vec 3 0)
(displayln vec)

(displayln #(1 2 3))
(displayln (vector-ref #(1 2 3) 0))
(displayln (vector-ref #(1 2 3) 1))
(displayln (vector-ref #(1 2 3) 2))

(displayln "equal")
(displayln (equal? #(1 2 3) #(1 2 3)))
(displayln (equal? #(1 2 3) #(2 2 3)))

(displayln "make-vector")
(displayln (make-vector 5))
(displayln (make-vector 5 3))
(displayln (make-vector 5 #f))
(displayln (make-vector 0))

