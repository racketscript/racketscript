#lang racket

(provide global-export-graph
         global-unreachable-idents)

;;;; Module dependencies and exports
;;;; Refer `moddeps.rkt`.

;; (Parameter (Maybe ExportGraph))
(define global-export-graph (make-parameter #f))

;; (MutableHashTable Module-Path (Setof Symbol))
(define global-unreachable-idents (make-hash))
