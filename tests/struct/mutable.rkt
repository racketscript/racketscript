#lang racket

(struct cell ([content #:mutable]) #:transparent)
(define a-cell (cell 0))

(equal? a-cell (cell 0))

(cell-content a-cell)
(set-cell-content! a-cell 1)
(cell-content a-cell)

(equal? a-cell (cell 0))
(equal? a-cell (cell 1))
(equal? a-cell a-cell)
