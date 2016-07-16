#lang racket

(define (square x) (* x x))

(define-values (prop:p p? p-ref) (make-struct-type-property 'p))
(define-values (struct:a make-a a? a-ref a-set!)
               (make-struct-type 'a #f 2 1 'uninitialized
                                 (list (cons prop:p 8))))
(p? struct:a)
(p? 13)

(define an-a (make-a 'x 'y))
(p? an-a)
(p-ref an-a)

(define-values (struct:b make-b b? b-ref b-set!)
               (make-struct-type 'b #f 0 0 #f))
(p? struct:b)


(define-values (prop:q q? q-ref) (make-struct-type-property
                                   'q (lambda (v si) (add1 v))
                                   (list (cons prop:p square))))
(define-values (struct:c make-c c? c-ref c-set!)
               (make-struct-type 'c #f 0 0 'uninit
                                 (list (cons prop:q 8))))

(q-ref struct:c)
(p-ref struct:c)
