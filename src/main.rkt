#lang racket/base

(require racket/cmdline
         racket/match
         racket/pretty
         racket/system
         racket/format
         threading
         "absyn.rkt"
         "expand.rkt"
         "config.rkt"
         "analyze.rkt"
         "transform.rkt"
         "assembler.rkt")

(define build-mode (make-parameter 'js))
(define rapture-dir (simplify-path (build-path (find-system-path 'orig-dir)
                                               (find-system-path 'run-file)
                                               "../..")))
(define js-bootstrap-file (make-parameter "bootstrap.js"))
(define js-output-file (make-parameter "compiled.js"))

(define (racket->js filename)
  (~> (quick-expand filename)
      (rename-program _)
      (absyn-top-level->il _)
      (assemble _)))

(define runtime-files
  (in-directory (build-path rapture-dir "src" "runtime")))

(define (prepare-build-directory dir)
  (define mkdir? 
    (cond
      [(directory-exists? dir) #f]
      [(file-exists? dir) (error "Output directory path is not a directory")]
      [else #t]))
  (when mkdir?
    (make-directory dir)
    (make-directory (build-path dir "modules")))


  (for ([f runtime-files])
    (define-values (base fname _) (split-path f))
    (copy-file f (build-path dir "modules" fname) #t))

  (copy-file (build-path rapture-dir "src" "js-support" (js-bootstrap-file))
             (build-path dir (js-bootstrap-file))
             #t))

(define (es6->es5 dir mod)
  #;(let ([compiled (build-path dir "modules" (js-output-file))])
    (when (file-exists? compiled)
      (delete-file compiled)))
  ;; TODO: Use NPM + some build tool to do this cleanly
  (parameterize ([current-directory (build-path dir "modules")])
    (system (~a "traceur" " --out " (js-output-file) " " mod ".js"))))
  
(module+ main
  (define source
    (command-line
     #:program "rapture"
     #:once-each
     [("-d" "--build-dir") dir "Output directory" (output-directory dir)]
     #:once-any
     ["--ast" "Expand and print AST" (build-mode 'absyn)]
     ["--il" "Compile to intermediate langauge (IL)" (build-mode 'il)]
     ["--js" "Compile to JS" (build-mode 'js)]
     #:args (filename) filename))

  (define expanded (quick-expand source))

  (match (build-mode)
    ['js (prepare-build-directory (output-directory))
         (~> (rename-program expanded)
             (absyn-top-level->il _)
             (assemble _))
         (es6->es5 (output-directory) (~a (Module-id expanded)))]
    ['il (~> (rename-program expanded)
             (absyn-top-level->il _)
             (pretty-print _))]
    ['absyn  (~> (rename-program expanded)
                 (pretty-print _))])

  (void))
