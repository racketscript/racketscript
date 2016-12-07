#lang racket

(define (check i j k)
  (equal? (+ (* i i) (* j j)) (* k k)))

(define (triplets n)
  (define results '())
  (let loop-i ([i 1])
    (let loop-j ([j 1])
      (let loop-k ([k 1])
        (when (check i j k)
          (set! results (cons (list i j k) results)))
        (when (< k n) (loop-k (add1 k))))
      (when (< j n) (loop-j (add1 j))))
    (when (< i n) (loop-i (add1 i))))
  results)

(displayln (triplets 100))
