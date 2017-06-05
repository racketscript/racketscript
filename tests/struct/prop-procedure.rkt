#lang racket

(struct annotated-proc (base note)
  #:property prop:procedure
  (struct-field-index base))

(define plus1 (annotated-proc
               (lambda (x) (+ x 1))
               "adds 1 to its argument"))

(procedure? plus1)
(annotated-proc? plus1)
(plus1 10)
(annotated-proc-note plus1)
