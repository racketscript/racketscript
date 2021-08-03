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

;; test bitwise operations
(displayln (bitwise-and 5 3)) ;; 1
(displayln (bitwise-and 5 3 -1983)) ;; 1
(displayln (bitwise-ior 5 3)) ;; 7
(displayln (bitwise-ior 5 3 10)) ;; 15
(displayln (bitwise-xor 5 3)) ;; 6
(displayln (bitwise-xor 5 3 10)) ;; 12
(displayln (bitwise-not 5)) ;; -6
(displayln (bitwise-not -3)) ;; 2
