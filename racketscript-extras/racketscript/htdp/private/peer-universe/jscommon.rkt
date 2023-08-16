#lang racketscript/base

(require (for-syntax racketscript/base
                     syntax/parse))

(provide :=
         *this*
         *null*
         *undefined*
         new
         define-proto
         set-object!
         schedule-method
         schedule-animation-frame
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
         half
         (rename-out [field-λ λ]))

;;-----------------------------------------------------------------------------
;; Interop helpers

(define-syntax  :=        (make-rename-transformer #'$/:=))
(define-syntax  new       (make-rename-transformer #'$/new))
(define-syntax *this*     (make-rename-transformer #'$/this))
(define-syntax *null*       (make-rename-transformer #'$/null))
(define-syntax *undefined*  (make-rename-transformer #'$/undefined))

(begin-for-syntax
  (define-syntax-class field
    #:description "a key-value pair for object"
    (pattern [name:id val:expr])))

(define-syntax (field-λ stx)
  (syntax-parse stx
    [(_ formals (~datum #:with-this) self:id body ...)
     #'(λ formals
         (define self *this*)
         body ...)]
    [(_ formals body ...) #'(λ formals body ...)]))

(define-syntax (define-proto stx)
  (syntax-parse stx
    [(define-proto name:id init:expr field:field ...)
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

(define document  #js*.window.document)
(define console   #js*.window.console)
(define Math      #js*.window.Math)
(define Path2D    #js*.window.Path2D)
(define abs+ceil  (λ (n) (abs (ceiling n))))

(define-syntax-rule (twice e)
  (* e 2))

(define-syntax-rule (half e)
  (/ e 2))