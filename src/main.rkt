#lang racket/base

(require racket/cmdline
         racket/file
         racket/format
         racket/match
         racket/list
         racket/path
         racket/port
         racket/pretty
         racket/runtime-path
         racket/set
         racket/system
         syntax/moddep

         data/queue
         threading

         "absyn.rkt"
         "il.rkt"
         "assembler.rkt"
         "config.rkt"
         "expand.rkt"
         "freshen.rkt"
         "global.rkt"
         "moddeps.rkt"
         "transform.rkt"
         "util.rkt")

(provide current-source-file
         main-source-file
         output-directory
         prepare-build-directory
         racket->js
         rapture-dir
         skip-gulp-build
         skip-npm-install)

(define build-mode (make-parameter 'js))
(define skip-npm-install (make-parameter #f))
(define skip-gulp-build (make-parameter #f))
(define js-output-file (make-parameter "compiled.js"))
(define js-bootstrap-file (make-parameter "bootstrap.js"))
(define dump-debug-info (make-parameter #f))

;; Compiler for ES6 to ES5 compilation.
;; - "babel"
;; - "traceur"
(define js-es6-compiler (make-parameter "traceur"))

(define-runtime-path rapture-main-module ".")

;; Path
;; Root directory of Rapture project
(define rapture-dir
  (~> rapture-main-module
      (path-only _)
      (build-path _ "..")
      (simplify-path _)))
(displayln (~a "Rapture root directory: " rapture-dir))

;; Path-String -> Path
;; Return path of support file named f
(define (support-file f)
  (build-path rapture-dir "src" "js-support" (js-es6-compiler) f))

;; PathString -> Path
;; Return path of runtime file named f
(define (runtime-file f)
  (build-path rapture-dir "src" "runtime" f))

;; Path-String -> Path
;; Return path of module file named f in output directory
(define (module-file f)
  (build-path (output-directory) "modules" f))

;; Path-String -> Path
;; Return path of support file named f
(define (package-file f)
  (build-path (output-directory) f))

;; Path-String Path-String -> Void
;; Copies file named fname to dest folder keeping
;; the original name same as before
(define (copy-file+ fname dest-dir)
  (define name (file-name-from-path fname))
  (copy-file fname (build-path dest-dir name) #t))

;; Path-String -> Path-String
(define (last-path-element p)
  (let-values ([(base last dir?) (split-path p)]) last))

;; Path-String Path-String -> Void
;; Copies directory from-dir inside to-dir *recursively*
(define (copy-directory from-dir to-dir)
  (define to-dir-dir (build-path to-dir (last-path-element from-dir)))
  (for ([f (in-directory from-dir)]
        #:unless (directory-exists? f))
    (define final-dest (build-path to-dir-dir (find-relative-path from-dir f)))
    (make-directory* (path-only final-dest))
    (copy-file f final-dest #t)))

;; Path-String Path-String (Listof Any) -> Void
;; Copies a Racket string patterned styled file `src` to `dest` file
;; with args applied to fill up format
(define (format-copy-file src dest args)
  (call-with-input-file src
    (λ (in)
      (call-with-output-file dest #:exists 'replace
        (λ (out)
          (fprintf out (apply format (port->string in) args)))))))

;; Path-String Path-String (Listof Any) -> Void
;; Like format-copy-file except that dest is a directory.
(define (format-copy-file+ src dest-dir args)
  (define name (file-name-from-path src))
  (format-copy-file src (build-path dest-dir name) args))

;; String -> Void
;; Puts a NPM and Gulp related files in output directory
;; with default-module set as the entry point module
;;
;; default-module is just the name of module excluding any file
;; extensions.
(define (copy-build-files default-module)
  (copy-file+ (support-file "package.json")
              (output-directory))
  (format-copy-file+ (support-file "gulpfile.js")
                     (output-directory)
                     (list default-module)))

;; -> Void
(define (copy-runtime-files)
  (copy-directory (build-path rapture-dir "src" "runtime")
                  (output-directory)))

;; -> Void
(define (copy-support-files)
  (when (equal? (js-es6-compiler) "traceur")
    (copy-file+ (support-file (js-bootstrap-file))
                (output-directory))))

;; String -> Void
;; Create output build directory tree with all NPM, Gulp. Runtime and
;; other support files
;;
;; default-module-name: is just the name of entry point module with
;; the file extension
(define (prepare-build-directory default-module-name)
  (define dir (output-directory))
  (define mkdir?
    (cond
      [(file-exists? dir) (error "Output directory path is not a directory")]
      [else #t]))
  (when mkdir?
    (make-directory* dir)
    (make-directory* (build-path dir "modules")))

  (copy-build-files default-module-name)
  (copy-runtime-files)
  (copy-support-files))

;; -> Void
;; Install and build dependenciese to translate ES5 to ES5
(define (es6->es5)
  ;; TODO: Use NPM + some build tool to do this cleanly
  (parameterize ([current-directory (output-directory)])
    (unless (skip-npm-install)
      (system "npm install"))
    (unless (skip-gulp-build)
      (system "gulp"))))

;;;; Generate stub module


(define (generate-stub-module mod)
  (parameterize ([current-source-file mod])
    (define idents (~> (hash-ref (used-idents) mod)
                       (set-map _ first)
                       (remove-duplicates _)))
    (define ilmod
      (ILModule #f
                (map ILProvide idents)
                (list (ILRequire (build-path "./" (substring (~a mod ".js") 2))
                                 '$$mod))
                (append
                 (list
                  (ILVarDec '$$ (ILRef '$$mod 'exports)))
                 (for/list ([id idents])
                   (ILVarDec id (ILIndex '$$ (ILValue (symbol->string id))))))))

    (define output-fpath (build-path (output-directory)
                                     "runtime"
                                     (substring (~a mod ".rkt.js") 2)))

    (call-with-output-file output-fpath
      #:exists 'replace
      (λ (out)
        (assemble-module ilmod out)))))

;; -> Void
;; For given global parameters starts build process starting
;; with entry point module and all its dependencies
(define (racket->js)
  (define added (mutable-set))
  (define pending (make-queue))

  (define (put-to-pending! mod)
    (unless (set-member? added mod)
      (set-add! added mod)
      (enqueue! pending mod)))

  (put-to-pending! (path->complete-path (main-source-file)))
  
  (let loop ()
    (cond
      [(queue-empty? pending)
       (printf "Writing stub modules.\n")
       (generate-stub-module '#%kernel)
       (es6->es5)
       (printf "Finished.\n")]
      [else
       (define next (dequeue! pending))
       (current-source-file next)

       (define expanded (quick-expand next))
       (define renamed (freshen expanded))
       (define ast (convert renamed (build-path next)))

       ;; build directories to output build folder.
       ;; TODO: Making directories after expanding and converting is weird
       (when (equal? next (main-source-file))
         (prepare-build-directory (~a (Module-id ast))))
       (make-directory* (path-only (module-output-file next)))

       (assemble-module (absyn-module->il ast) #f)

       (for ([mod (in-set (Module-imports ast))])
         (match mod
           [(? symbol? _) (void)]
           [_ #:when (collects-module? mod) (void) #;(put-to-pending! mod)]
           [_ (put-to-pending! mod)]))
       (loop)])))

(module+ main
  (define source
    (command-line
     #:program "rapture"
     #:usage-help "Compile Racket to JavaScript"
     #:once-each
     [("-d" "--build-dir") dir "Output directory" (output-directory (simplify-path dir))]
     [("-n" "--skip-npm-install") "Skip NPM install phase" (skip-npm-install #t)]
     [("-g" "--skip-gulp-build") "Skip Gulp build phase" (skip-gulp-build #t)]
     ["--stdout" "Print compiled JS to standard output" (print-to-stdout #t)]
     ["--dump-debug-info" "Dumps some debug information in output directory"
      (dump-debug-info #t)]
     #:multi
     ["--es6" compiler "ES6 to ES5 compiler"
      (if (or (equal? compiler "traceur") (equal? compiler "babel"))
          (js-es6-compiler compiler)
          (error "`~a` is not a supported ES6 compiler"))]
     #:once-any
     ["--expand" "Fully expand Racket source" (build-mode 'expand)]
     ["--ast" "Expand and print AST" (build-mode 'absyn)]
     ["--rename" "Expand and print AST after α-renaming" (build-mode 'rename)]
     ["--il" "Compile to intermediate langauge (IL)" (build-mode 'il)]
     ["--js" "Compile to JS" (build-mode 'js)]
     #:args (filename)
     (let ([complete-filename (path->complete-path filename)])
       (current-source-file complete-filename)
       (main-source-file complete-filename)
       complete-filename)))

  ;; Initialize global-export-graph so that we can import each
  ;; module as an object and follow identifier's from there.
  (display "Resolving module dependencies and identifiers... ")
  (global-export-graph (get-export-tree source))
  (displayln "Done!")

  (match (build-mode)
    ['expand (~> (quick-expand source)
                 (syntax->datum _)
                 (pretty-print _))]
    ['absyn (~> (quick-expand source)
                (freshen _)
                (convert _ (build-path source))
                (pretty-print _))]
    ['rename  (~> (quick-expand source)
                  (freshen _)
                  (syntax->datum _)
                  (pretty-print _))]
    ['il (~> (quick-expand source)
             (freshen _)
             (convert _ (build-path source))
             (absyn-module->il _)
             (pretty-print _))]
    ['js (racket->js)])

  ;; Dump debug information
  (when (dump-debug-info)
    (with-output-to-file (build-path (output-directory) "debug.txt") #:exists 'truncate
      (λ ()
        (pretty-print (used-idents)))))

  (void))
