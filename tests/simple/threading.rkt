#lang racket

;; To check if third party imports work

(require threading)

(~> '(1 2 3 4 5)
    (map (λ (x) (* x x )) _)
    (map (λ (x) (- x (/ x x))) _)
    (displayln _))
