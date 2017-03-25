#lang racket

(provide foo)
(provide (for-meta 1 foo))
(provide (for-meta 2 foo))

(begin-for-syntax
  (require racket)
  (begin-for-syntax
    (define (foo)
      (displayln"Phase 2")))

  (define (foo)
    (displayln "Phase 1!")))

(define (foo)
  (displayln "Phase 0!"))
