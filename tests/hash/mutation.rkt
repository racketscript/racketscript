#lang racket/base

;; TODO: Add more tests around vectors.

(define (rr h v*)
  (displayln (map (λ (v) (hash-ref h v #f)) v*)))

(let ()
  (struct posn ([x #:mutable] y) #:transparent)
  (define p1 (posn 1 2))
  (define p2 (posn 1 2))
  (equal? p1 p2)
  (eqv? p1 p2)
  (eq? p1 p2)

  (set-posn-x! p1 11)
  (equal? p1 p2)
  (eqv? p1 p2)
  (eq? p1 p2))

(let ()
  (define s1 "AB")
  (define s2 "ABAB")
  (define s3 (format "~a~a" s1 s1))

  (rr (hash s1 1 s2 2 s3 3) (list s1 s2 s3))
  (rr (hasheqv s1 1 s2 2 s3 3) (list s1 s2 s3))
  (rr (hasheq s1 1 s2 2 s3 3) (list s1 s2 s3)))


(let ()
  (define m1-1 (mcons 1 2))
  (define m1-2 (mcons 1 2))
  (define m2-1 (mcons 3 4))
  (define m2-2 (mcons 3 4))

  (define l1 (list m1-1 m2-1))
  (define l2 (list m1-2 m2-2))

  (define h1 (hash l1 1 l2 2))
  (define h2 (hasheqv l1 1 l2 2))
  (define h3 (hasheq l1 1 l2 2))

  (rr h1 (list l1 l2))
  (rr h2 (list l1 l2))
  (rr h3 (list l1 l2))

  (rr h1 (list (mcons 1 2) (mcons 3 4)))
  (rr h2 (list (mcons 1 2) (mcons 3 4)))
  (rr h3 (list (mcons 1 2) (mcons 3 4)))

  (set-mcar! m1-1 11)
  (rr h1 (list l1 l2))
  (rr h2 (list l1 l2))
  (rr h3 (list l1 l2))

  (rr h1 (list (mcons 11 2)))
  (rr h2 (list (mcons 11 2)))
  (rr h3 (list (mcons 11 2)))

  (rr h1 (list (mcons 1 2) (mcons 3 4)))
  (rr h2 (list (mcons 1 2) (mcons 3 4)))
  (rr h3 (list (mcons 1 2) (mcons 3 4))))


(let ()
  (struct posn ([x #:mutable] y) #:transparent)

  (define p1 (posn 1 2))
  (define p2 (posn 3 4))

  (define h1 (hash p1 1 p2 2))
  (define h2 (hasheqv p1 1 p2 2))
  (define h3 (hasheq p1 1 p2 2))

  (rr h1 (list p1 p2))
  (rr h2 (list p1 p2))
  (rr h3 (list p1 p2))

  (rr h1 (list (posn 1 2) (posn 3 4)))
  (rr h2 (list (posn 1 2) (posn 3 4)))
  (rr h3 (list (posn 1 2) (posn 3 4)))

  (set-posn-x! p1 11)

  (rr h1 (list p1 p2))
  (rr h2 (list p1 p2))
  (rr h3 (list p1 p2))

  (rr h1 (list (posn 1 2) (posn 3 4)))
  (rr h2 (list (posn 1 2) (posn 3 4)))
  (rr h3 (list (posn 1 2) (posn 3 4)))

  (rr h1 (list (posn 11 2)))
  (rr h2 (list (posn 11 2)))
  (rr h3 (list (posn 11 2))))
