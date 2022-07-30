#lang racket/base

(require "../private/interop.rkt"
         "set.rkt")

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
         ignored-module-imports-in-boot
         ignored-undefined-identifier?

         skip-arity-checks?)

;;; ---------------------------------------------------------------------------
(define FFI-CALL-ID '#%js-ffi)

(define output-directory  (make-parameter "js-build"))

(define current-source-file (make-parameter #f))

(define main-source-file (make-parameter #f))

(define (cache-directory)
  (let ([dir (build-path (output-directory) "cache")])
    (if (directory-exists? dir)
        dir
        (begin (make-directory dir)
               dir))))

;; Path to the main compiler module

(define racketscript-main-module (path->complete-path "main.rkt"))

(define racketscript-compiler-dir
  (let-values ([(dir-path _p _b) (split-path racketscript-main-module)])
    dir-path))

(define racketscript-runtime-dir
  (build-path racketscript-compiler-dir "runtime"))

;; Root directory of Racketscript project
(define racketscript-dir
  (simplify-path (build-path racketscript-compiler-dir "..")))

;;; ---------------------------------------------------------------------------

;; Name of kernel/core module object used in JavaScript environment
(define jsruntime-core-module (make-parameter "$rjs_core"))

(define (jsruntime-module-path mod)
  (let ([mod-name (cond
                    [(eq? mod 'core) "core.js"]
                    [(set-member? primitive-modules mod)
                     (string-append
                       (substring (symbol->string mod) 2)
                       ".rkt.js")])])
    (path->complete-path
     (build-path (output-directory)
                 "runtime"
                 mod-name))))

;;; ---------------------------------------------------------------------------

(define test-environment? (make-parameter #f))

(define logging? (make-parameter #t))

;; Ignore these imports in primtive modules.
(define ignored-module-imports-in-boot
  (set
   (build-path racketscript-dir "private" "interop.rkt")))

(define ignored-undefined-identifiers
  (list #'#%js-ffi))

(define (ignored-undefined-identifier? id)
  (ormap (λ (ignored) (free-identifier=? id ignored)) ignored-undefined-identifiers))

(define primitive-modules
  (set '#%runtime
       '#%core
       '#%main
       '#%read
       '#%kernel
       '#%paramz
       '#%unsafe
       '#%utils
       '#%flfxnum
       '#%futures
       '#%extfl
       '#%place-struct
       '#%network
       '#%builtin
       '#%boot
       '#%foreign
       '#%place
       '#%linklet-primitive
       (build-path racketscript-runtime-dir "lib.rkt")))

;;; ---------------------------------------------------------------------------

(define skip-arity-checks? (make-parameter #f))
