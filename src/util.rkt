#lang racket

(provide hash-set*
         hash*)

(define (hash-set* h . kvs)
  (let loop ([kvs kvs] [h h])
    (if (null? kvs)
        h
        (let* ([k (car kvs)]
               [v (cadr kvs)]
               [h (if v (hash-set h k v) h)])
          (loop (cddr kvs) h)))))

(define (hash* . kvs) (apply hash-set* (hash) kvs))

