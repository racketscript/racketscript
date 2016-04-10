#lang typed/racket/base

(provide output-directory
         print-to-stdout
         jsruntime-kernel-module
         jsruntime-core-module)

(define output-directory (make-parameter "js-build"))
(define jsruntime-kernel-module (make-parameter "$rjs_kernel"))
(define jsruntime-core-module (make-parameter "$rjs_core"))

(: print-to-stdout (Parameter Boolean))
(define print-to-stdout (make-parameter #f))
