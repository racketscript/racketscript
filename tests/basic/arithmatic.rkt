#lang racket/base
(require "../test-utils.rkt")

;; Division is not included in this test case
;; as semantics of numbers are not preserved

(displayln (+ 1 2 3 4 5))
(displayln (- 1 2 3 4 5))
(displayln (* 1 2 3 4 5))
(displayln (/ 4 2))

(displayln (+ 2))
(displayln (- 2))
(displayln (- (- 2)))
(displayln (* 2))

(displayln (< 1 2 3 4 5))
(displayln (> 1 2 3 4 5))
(displayln (= 1 2 3 4 5))
(displayln (= 1 2 3 10 5 8))

(displayln (negative? -1))
(displayln (negative? 1))
(displayln (positive? 1))
(displayln (positive? -1))
(displayln (< -1 0))
(displayln (> -1 0))
(displayln (<= -1 0))
(displayln (>= -1 0))

(displayln (abs 1))
(displayln (abs -1))

;; test multiple arities
;(displayln (+)) ;; TODO
(displayln (+ 1))
(displayln (- 1))
(displayln (* 1))
(displayln (/ 1))
(displayln (+ 1 2 3))
(displayln (- 1 2 3))
(displayln (* 1 2 3))
(displayln (/ 1 1 1))
(run-if-version "7.0.0.13" (displayln (= 1)))
(run-if-version "7.0.0.13" (displayln (< 1)))
(run-if-version "7.0.0.13" (displayln (<= 1)))
(run-if-version "7.0.0.13" (displayln (> 1)))
(run-if-version "7.0.0.13" (displayln (>= 1)))
(displayln (= 1 2 3))
(displayln (< 1 2 3))
(displayln (<= 1 2 3))
(displayln (> 1 2 3))
(displayln (>= 1 2 3))

(displayln (+))
(displayln (*))
