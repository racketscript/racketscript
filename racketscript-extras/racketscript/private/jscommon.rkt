#lang racketscript/base

(require (for-syntax racketscript/base
                     syntax/parse))

(provide :=
         new
         define-proto
         set-object!
         schedule-method
         schedule-animation-frame
         ++
         document
         console
         Math
         Path2D
         abs
         sin
         cos
         floor
         abs+ceil
         max
         min
         twice
         half)

;;-----------------------------------------------------------------------------
;; Interop helpers

(define-syntax  :=        (make-rename-transformer #'$/:=))
(define-syntax  new       (make-rename-transformer #'$/new))

(begin-for-syntax
  (define-syntax-class field
    #:description "a key-value pair for object"
    (pattern [name:id val:expr])))

(define-syntax (define-proto stx)
  (syntax-parse stx
    [(define-proto name:id (~datum #:init) init:expr
       (~optional (~seq (~datum #:prototype-fields) field:field ...)))
     #`(begin
         (define name init)
         #,(when (attribute field)
             #`(begin
                 (:= ($ name 'prototype 'field.name) field.val) ...)))]))

(define-syntax (set-object! stx)
  (syntax-parse stx
    [(set-object! obj:expr f:field ...)
     #`(begin (:= ($ obj 'f.name) f.val) ...)]))


(define-syntax-rule (schedule-method this method interval)
  (let ([self this])
    (#js*.window.setTimeout (λ ()
                             (($ self method)))
                            interval)))

(define-syntax-rule (schedule-animation-frame this step)
  (let ([self this])
    (#js*.window.requestAnimationFrame (λ ()
                                         (($ self step))))))

;;-----------------------------------------------------------------------------
;; Helper functions

(define ++        string-append)
(define document  #js*.window.document)
(define console   #js*.window.console)
(define Math      #js*.window.Math)
(define Path2D    #js*.window.Path2D)
(define abs+ceil  (λ (n) (abs (ceiling n))))

(define-syntax-rule (twice e)
  (* e 2))

(define-syntax-rule (half e)
  (/ e 2))
