#lang racket

;; Tests dependecy caching by compiling once, then swapping
;; the names of ./private/dependency.rkt and ./private/depndency-changed.rkt,
;; then compiling again. If dependency caching isn't working properly, there
;; will be an error on the second compilation.

(require "./private/dependency.rkt")

(println (format "~a" (add 5)))
