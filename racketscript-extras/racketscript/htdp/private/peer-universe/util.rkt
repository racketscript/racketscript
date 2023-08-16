#lang racketscript/base

(require (for-syntax racketscript/base
                     syntax/parse))

(provide format-js-str
         (all-defined-out))

(define-syntax-rule (format-js-str fmt-str args ...)
  (js-string (format fmt-str args ...)))

(define (js-string? s)
  (or ($/typeof s "string") ($/instanceof s #js*.String)))

;; NOTE: because every racket datatype in 
;;       racketscript is stored as a js object,
;;       ($/typeof obj <any racket variable>)
;;       will always be true
(define (js-object? obj)
  (and (not (string? obj)
            (number? obj)
            (boolean? obj)
            (list? obj)
            (symbol? obj))
       ($/typeof obj "object")))

(define (null? val)
  ($/binop === val $/null))

(define (undefined? val)
  ($/binop === val $/undefined))

(define (js-array? arr)
  (#js*.Array.isArray arr))

(define (msg->string msg)
  (cond [(undefined? msg)       "undefined"]
        [(js-string? msg)       (js-string->string msg)]
        [(or (js-object? msg)
             (js-array?  msg)
             (null?      msg))  (#js*.JSON.stringify msg)]
        [else (format "~a" msg)]))