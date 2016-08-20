#lang typed/racket/base

(require racket/match)

(provide output-directory
         print-to-stdout
         racket-collects-dir
         current-source-file
         main-source-file
         FFI-CALL-ID
         test-environment?
         jsruntime-module-path
         jsruntime-core-module)

;;; ---------------------------------------------------------------------------
(define FFI-CALL-ID '#%js-ffi)

(: output-directory (Parameter String))
(define output-directory  (make-parameter "js-build"))

(: current-source-file (Parameter (Option Path)))
(define current-source-file (make-parameter #f))

(: main-source-file (Parameter (Option Path)))
(define main-source-file (make-parameter #f))

;;; ---------------------------------------------------------------------------

(: jsruntime-core-module (Parameter String))
;; Name of kernel/core module object used in JavaScript environment
(define jsruntime-core-module (make-parameter "$rjs_core"))

(: jsruntime-module-path (-> Symbol Path))
(define (jsruntime-module-path mod)
  (let ([mod-name (match mod
                    ['#%kernel "kernel.rkt.js"]
                    ['#%utils "utils.rkt.js"]
                    ['#%unsafe "unsafe.rkt.js"]
                    ['#%flfxnum "flfxnum.rkt.js"]
                    ['core "core.js"]
                    [_ "rest.rkt.js"])])
    (path->complete-path
     (build-path (output-directory)
                 "runtime"
                 mod-name))))

;;; ---------------------------------------------------------------------------

(: racket-collects-dir (Parameter Path))
(define racket-collects-dir (make-parameter
                             (build-path "/usr/share/racket/collects")))

(: print-to-stdout (Parameter Boolean))
(define print-to-stdout (make-parameter #f))

(: test-environment? (Parameter Boolean))
(define test-environment? (make-parameter #f))
