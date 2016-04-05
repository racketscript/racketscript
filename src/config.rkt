#lang typed/racket

(provide output-directory
         jsruntime-kernel-module
         jsruntime-core-module)

(define output-directory (make-parameter "js-build"))
(define jsruntime-kernel-module (make-parameter "$rjs_kernel"))
(define jsruntime-core-module (make-parameter "$rjs_core"))
