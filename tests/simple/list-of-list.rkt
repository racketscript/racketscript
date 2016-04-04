#lang racket

(define (append list1 list2)
  (if (null? list1) list2
    (cons (car list1)
          (append (cdr list1) list2))))

(define (flatten lst)
  (cond
    [(empty? lst) '()]
    [else (let ([hd (first lst)]
                [tl (rest lst)])
            (if (list? hd)
              (append (flatten hd) (flatten tl))
              (cons hd (flatten tl))))]))

(displayln (flatten '(1 2 3 4)))
(displayln (flatten '(1 (2 3 4 5) 6)))
(displayln (flatten '(1 (2 3 4 5) (6 7 8 9))))
(displayln (flatten '(1 (2 3 4 5) (6 7 8 (9 10 11 (12 13) 14)))))
(displayln '(1 (2 3 4 5) (6 7 8 (9 10 11 (12 13) 14))))

