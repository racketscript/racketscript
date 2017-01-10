#lang typed/racket/base

(require racket/match
         racket/path
         racket/runtime-path
         racket/set
         threading)

(provide output-directory
         logging?
         current-source-file
         main-source-file
         FFI-CALL-ID
         test-environment?

         racketscript-dir
         racketscript-compiler-dir
         racketscript-runtime-dir
         cache-directory

         jsruntime-module-path
         jsruntime-core-module

         primitive-modules
         ignored-module-imports-in-boot)

;;; ---------------------------------------------------------------------------
(define FFI-CALL-ID '#%js-ffi)

(: output-directory (Parameter Path-String))
(define output-directory  (make-parameter "js-build"))

(: current-source-file (Parameter (Option (U Path Symbol))))
(define current-source-file (make-parameter #f))

(: main-source-file (Parameter (Option Path)))
(define main-source-file (make-parameter #f))

(: cache-directory (-> Path))
(define (cache-directory)
  (let ([dir (build-path (output-directory) "cache")])
    (if (directory-exists? dir)
        dir
        (begin (make-directory dir)
               dir))))

;; Path to the main compiler module
(define-runtime-path racketscript-main-module "main.rkt")

(: racketscript-compiler-dir Path)
(define racketscript-compiler-dir
  (cast (path-only racketscript-main-module) Path))

(: racketscript-runtime-dir Path)
(define racketscript-runtime-dir
  (build-path racketscript-compiler-dir "runtime"))

(: racketscript-dir Path)
;; Root directory of Racketscript project
(define racketscript-dir
  (~> racketscript-compiler-dir
      (build-path _ "..")
      (simplify-path _)))

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
                    ['#%paramz "paramz.rkt.js"]
                    ['#%flfxnum "flfxnum.rkt.js"]
                    ['core "core.js"]
                    [_ "rest.rkt.js"])])
    (path->complete-path
     (build-path (output-directory)
                 "runtime"
                 mod-name))))

;;; ---------------------------------------------------------------------------

(: test-environment? (Parameter Boolean))
(define test-environment? (make-parameter #f))

(: logging? (Parameter Boolean))
(define logging? (make-parameter #t))

(: ignored-module-imports-in-boot (Setof Path))
;; Ignore these imports in primtive modules.
(define ignored-module-imports-in-boot
  (set
   (build-path racketscript-dir "private" "interop.rkt")))


(: primitive-modules (Setof Symbol))
(define primitive-modules
  (set '#%kernel
       '#%utils
       '#%paramz
       '#%unsafe))
