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

(with-handlers ([exn:fail:contract?
                 (λ (e)
                   (displayln
                    (regexp-match "mutable" (exn-message e))))])
  (hash-set! (hash) 1 2))

(with-handlers ([(λ (x)
                   (displayln 'checking)
                   (exn:fail:contract? x))
                 (λ (e)
                   (displayln 'caught)
                   (displayln (exn-message e))
                   ;; TODO: improve racket->js regexp translation
                   (displayln (regexp-match "foldl" (exn-message e)))
                   (displayln (regexp-match "contract violation" (exn-message e)))
                   (displayln (regexp-match "expected" (exn-message e)))
                   (displayln (regexp-match "procedure" (exn-message e)))
                   (displayln (regexp-match "given" (exn-message e)))
                   (displayln (regexp-match "\\+" (exn-message e))))])
  (foldl '+ 0 (build-list 10 add1)))

(with-handlers ([exn:fail:contract? (λ (e) (displayln (exn-message e)))])
  (foldl 'list 0 10))

(with-handlers ([exn:fail:contract? (λ (e) (displayln (exn-message e)))])
  (foldl list 0 10))

;; arity
(with-handlers ([exn:fail:contract? (λ (e) (displayln (exn-message e)))])
  (foldl add1 0 '(1 2)))
