#lang racket/base

(require racket/format)

(define (~p a b)
  (display a)
  (display "   => ")
  (display b)
  (displayln ""))

(~p '(+ 1 2 3 4 5) (+ 1 2 3 4 5))
(~p '(- 1 2 3 4 5) (- 1 2 3 4 5))
(~p '(* 1 2 3 4 5) (* 1 2 3 4 5))
(~p '(/ 1 2 3 4 5) (/ 1 2 3 4 5))
(displayln "")

(~p '(+ 2) (+ 2))
(~p '(- 2) (- 2))
(~p '(* 2) (* 2))
(~p '(/ 2) (/ 2))
(displayln "")

(~p '(< 1 2 3 4 5) (< 1 2 3 4 5))
(~p '(> 1 2 3 4 5) (> 1 2 3 4 5))
(~p '(= 1 2 3 4 5) (= 1 2 3 4 5))
(~p '(= 1 2 3 10 5 8) (= 1 2 3 10 5 8))
(displayln "")

