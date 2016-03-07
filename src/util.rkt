#lang racket

(provide hash-set-pair*
         hash*)

(define (hash-set-pair* h pairs)
  (let loop ([p* pairs] [h h])
    (if (empty? p*)
        h
        (let* ([p (car p*)]
               [k (car p)]
               [v (cdr p)])
          (loop (cdr p*) (hash-set h k v))))))

(define (hash* . kvs) (apply hash-set* (hash) kvs))

