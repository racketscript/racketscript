#lang racket

(define (append list1 list2)
  (if (null? list1) list2
    (cons (car list1)
          (append (cdr list1) list2))))

(define (reverse-list lst)
  (cond
    [(empty? lst) '()]
    [else (append (reverse-list (rest lst))
                  (list (first lst)))]))

(displayln (reverse-list (list 1 2 3 4 5)))
