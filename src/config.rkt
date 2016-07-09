#lang typed/racket/base

(provide output-directory
         print-to-stdout
         racket-collects-dir
         current-source-file
         main-source-file
         FFI-CALL-ID
         test-environment?
         jsruntime-core-module
         jsruntime-kernel-module
         jsruntime-core-module-path
         jsruntime-kernel-module-path)

;;; ---------------------------------------------------------------------------
(define FFI-CALL-ID '#%js-ffi)

(: output-directory (Parameter String))
(define output-directory  (make-parameter "js-build"))

(: current-source-file (Parameter (Option Path)))
(define current-source-file (make-parameter #f))

(: main-source-file (Parameter (Option Path)))
(define main-source-file (make-parameter #f))

;;; ---------------------------------------------------------------------------

(: jsruntime-kernel-module (Parameter String))
(: jsruntime-core-module (Parameter String))
;; Name of kernel/core module object used in JavaScript environment
(define jsruntime-kernel-module (make-parameter "$rjs_kernel"))
(define jsruntime-core-module (make-parameter "$rjs_core"))

(: jsruntime-kernel-module-path (-> Path))
(define (jsruntime-kernel-module-path)
  (path->complete-path
   (build-path (output-directory)
               "runtime"
               "kernel.js")))

(: jsruntime-core-module-path (-> Path))
(define (jsruntime-core-module-path)
  (path->complete-path
   (build-path (output-directory)
               "runtime"
               "core.js")))

;;; ---------------------------------------------------------------------------

(: racket-collects-dir (Parameter Path))
(define racket-collects-dir (make-parameter
                             (build-path "/usr/share/racket/collects")))

(: print-to-stdout (Parameter Boolean))
(define print-to-stdout (make-parameter #f))

(: test-environment? (Parameter Boolean))
(define test-environment? (make-parameter #f))
