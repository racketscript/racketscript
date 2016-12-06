#lang racket

;; To check if commonly imported modules are re-evaluated in each test.
;; using our new runner.

(provide x)

(define x (box 0))
