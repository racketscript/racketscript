#lang racket/base
(require racket/match syntax/stx)
(provide (all-defined-out))

(define (stx-foldl f b . lsts)
  (apply foldl f b (map stx->list lsts)))

;; like stx-foldl but accum is first arg of f instead of last
(define (stx-foldl* f b . lsts)
  (apply stx-foldl (match-lambda* [(list xs ... y) (apply f y xs)]) b lsts))
