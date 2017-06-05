#lang racket/base

(define (hello str)
  (displayln str))

(provide (rename-out [hello say-hello]))
