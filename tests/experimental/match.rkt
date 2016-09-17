#lang racket

(define (map fn lst)
  (match lst
    ['() '()]
    [(cons hd tl) (cons (fn hd)
                        (map fn tl))]))

(displayln (map (λ (x) (* x x)) '(1 2 3 4 5 6)))

;; struct

(struct posn-2d (x y))
(struct posn-3d (x y z))

(define (sqrt n)
  n)

(define (distance-from-origin p)
  (define (square x) (* x x))

  (match p
    [(posn-2d x y) (sqrt
                     (+ (square x)
                        (square y)))]
    [(posn-3d x y z) (sqrt (+ (square x)
                              (square y)
                              (square z)))]))


(displayln (distance-from-origin (posn-2d 3 4)))
(displayln (distance-from-origin (posn-3d 3 4 5)))
