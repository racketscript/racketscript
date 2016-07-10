#lang racket

(provide append
         reverse
         foldl
         map)

(define (append list1 list2)
  (if (null? list1) list2
    (cons (car list1)
          (append (cdr list1) list2))))

(define (reverse-list lst)
  (cond
    [(empty? lst) '()]
    [else (append (reverse-list (rest lst))
                  (list (first lst)))]))
(define (map fn lst)
  (cond
    [(null? lst) '()]
    [else (cons (fn (car lst))
                (map fn (cdr lst)))]))

(define (foldl fn acc lst)
  (cond
    [(null? lst) acc]
    [else (foldl fn (fn (first lst) acc) (rest lst))]))
