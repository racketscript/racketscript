#lang racket/base

(displayln "Simple with-handlers")
(with-handlers ([number? (lambda (n)
                           (+ n 5))])
  (raise 18 #t))


(displayln "Branched with-handlers")
(define (branched n)
  (with-handlers ([number? (lambda (n)
                             (displayln (list "got number: " n)))]
                  [string? (lambda (s)
                             (displayln (list "got string: " s)))]
                  [symbol? (lambda (s)
                             (displayln (list "got symbol: " s)))])
    (case n
      [(0) (raise 10)]
      [(1) (raise "hello")]
      [(2) (raise 'world)])))

(branched 0)
(branched 1)
(branched 2)

(displayln "Nested with-handlers")
(define (nested n)
  (with-handlers ([number? (lambda (n)
                             (displayln (list "got number: " n)))])
    (with-handlers ([string? (lambda (s)
                               (displayln (list "got string: " s)))])
      (with-handlers ([symbol? (lambda (s)
                                 (displayln (list "got symbol: " s)))])
        (case n
          [(0) (raise 10)]
          [(1) (raise "hello")]
          [(2) (raise 'world)])))))

(nested 0)
(nested 1)
(nested 2)
