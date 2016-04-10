#lang racket/base

(require racket/cmdline
         racket/match
         racket/pretty
         racket/system
         racket/format
         racket/path
         racket/file
         racket/port
         threading
         "absyn.rkt"
         "expand.rkt"
         "config.rkt"
         "analyze.rkt"
         "transform.rkt"
         "assembler.rkt")

(define build-mode (make-parameter 'js))
(define skip-npm-install (make-parameter #f))
(define skip-gulp-build (make-parameter #f))
(define js-output-file (make-parameter "compiled.js"))
(define js-bootstrap-file (make-parameter "bootstrap.js"))
(define racket-collects-dir (make-parameter "/usr/share/racket/collects"))

(define rapture-dir
  (~> (let ([dir (find-system-path 'orig-dir)]
            [file (find-system-path 'run-file)])
        (if (absolute-path? dir)
            file
            (build-path dir file)))
      (path-only _)
      (build-path _ "..")
      (simplify-path _)))

(define (support-file f)
  (build-path rapture-dir "src" "js-support" f))

(define (runtime-file f)
  (build-path rapture-dir "src" "runtime" f))

(define (module-file f)
  (build-path (output-directory) "modules" f))

(define (package-file f)
  (build-path (output-directory) f))

(define (copy-file+ fname dest)
  (define name (file-name-from-path fname))
  (copy-file fname (build-path dest name) #t))

(define (format-copy-file src dest args)
  (call-with-input-file src
    (λ (in)
      (call-with-output-file dest #:exists 'replace
        (λ (out)
          (fprintf out (apply format (port->string in) args)))))))

(define (format-copy-file+ src dest args)
  (define name (file-name-from-path src))
  (format-copy-file src (build-path dest name) args))

(define (racket->js filename)
  (~> (quick-expand filename)
      (rename-program _)
      (absyn-top-level->il _)
      (assemble _)))

(define runtime-files
  #;(in-directory (build-path rapture-dir "src" "runtime")) ;; FIX: This blows up with backup files by editor
  (list (runtime-file "core.js")
        (runtime-file "kernel.js")))

(define (copy-build-files default-module)
  (copy-file+ (support-file "package.json")
              (output-directory))
  (format-copy-file+ (support-file "gulpfile.js")
                     (output-directory)
                     (list default-module)))

(define (copy-runtime-files)
  (for ([f runtime-files])
    (define fname (file-name-from-path f))
    (copy-file f (module-file fname) #t)))

(define (copy-support-files)
  (copy-file+ (support-file (js-bootstrap-file))
              (output-directory)))

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

(define (es6->es5 dir mod)
  #;(let ([compiled (build-path dir "modules" (js-output-file))])
      (when (file-exists? compiled)
        (delete-file compiled)))
  ;; TODO: Use NPM + some build tool to do this cleanly
  (parameterize ([current-directory (output-directory)])
    (unless (skip-npm-install)
      (system "npm install"))
    (unless (skip-gulp-build)
      (system "gulp"))))
  
(module+ main
  (define source
    (command-line
     #:program "rapture"
     #:usage-help "Compile Racket to JavaScript"
     #:once-each
     [("-d" "--build-dir") dir "Output directory" (output-directory (simplify-path dir))]
     [("-n" "--skip-npm-install") "Skip NPM install phase" (skip-npm-install #t)]
     [("-g" "--skip-gulp-build") "Skip Gulp build phase" (skip-gulp-build #t)]
     #:once-any
     ["--expand" "Fully expand Racket source" (build-mode 'expand)]
     ["--ast" "Expand and print AST" (build-mode 'absyn)]
     ["--ast-rename" "Expand and print AST after α-renaming" (build-mode 'absyn-rename)]
     ["--il" "Compile to intermediate langauge (IL)" (build-mode 'il)]
     ["--js" "Compile to JS" (build-mode 'js)]
     #:args (filename) filename))

  (define expanded (quick-expand source))
  (define ast (convert expanded (build-path source)))

  (match (build-mode)
    ['expand (pretty-print (syntax->datum ast))]
    ['js (prepare-build-directory (~a (Module-id ast)))
         (~> (rename-program ast)
             (absyn-top-level->il _)
             (assemble _))
         (es6->es5 (output-directory) (~a (Module-id ast)))]
    ['il (~> (rename-program ast)
             (absyn-top-level->il _)
             (pretty-print _))]
    ['absyn (pretty-print ast)]
    ['absyn-rename  (~> (rename-program ast)
                        (pretty-print _))])

  (void))
