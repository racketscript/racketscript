#lang typed/racket/base

(provide output-directory
         print-to-stdout
         racket-collects-dir
         current-source-file
         main-source-file
         ffi-call-id
         jsruntime-core-module
         jsruntime-kernel-module
         jsruntime-core-module-path
         jsruntime-kernel-module-path)

(define output-directory (make-parameter "js-build"))

(: current-source-file (Parameter (Option Path)))
(define current-source-file (make-parameter #f))

(: main-source-file (Parameter (Option Path)))
(define main-source-file (make-parameter #f))

(define jsruntime-kernel-module (make-parameter "$rjs_kernel"))
(define jsruntime-core-module (make-parameter "$rjs_core"))

(define (jsruntime-kernel-module-path)
  (path->complete-path (build-path (output-directory) "runtime" "kernel.js")))

(define (jsruntime-core-module-path)
  (path->complete-path (build-path (output-directory) "runtime" "core.js")))

(: print-to-stdout (Parameter Boolean))
(define print-to-stdout (make-parameter #f))

(define racket-collects-dir (make-parameter
                             (build-path "/usr/share/racket/collects")))

(define ffi-call-id (make-parameter '$))
