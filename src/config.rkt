#lang typed/racket/base

(provide output-directory
         print-to-stdout
         racket-collects-dir
         jsruntime-core-module
         jsruntime-kernel-module
         jsruntime-kernel-module-path)

(define output-directory (make-parameter "js-build"))
(define jsruntime-kernel-module (make-parameter "$rjs_kernel"))
(define jsruntime-core-module (make-parameter "$rjs_core"))

(define jsruntime-kernel-module-path (make-parameter "../runtime/kernel.js"))

(: print-to-stdout (Parameter Boolean))
(define print-to-stdout (make-parameter #f))

(define racket-collects-dir (make-parameter "/usr/share/racket/collects"))
