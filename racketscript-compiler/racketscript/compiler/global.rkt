#lang racket

(provide global-export-graph)

;;;; Module dependencies and exports
;;;; Refer `moddeps.rkt`.

;; (Parameter (Maybe ExportGraph))
(define global-export-graph (make-parameter #f))
