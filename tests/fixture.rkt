#!/usr/bin/env racket
#lang racket

(require rackunit
         glob
         racket/runtime-path
         racketscript/compiler/main
         racketscript/compiler/util
         racketscript/compiler/global
         racketscript/compiler/moddeps
         racketscript/compiler/il-analyze)

;; Print Racket and JS output of test programs to stdout
;; Also show check failure
(define verbose? (make-parameter #f))

;; Do a complete cleanup for previous build directory before starting
;; tests.
(define clean-output-before-test (make-parameter #f))

;; Turning if false would ignore all standard output
;; produced by compiler
(define racketscript-stdout? (make-parameter #f))

(define (memoize lam)
  (let ([cache (make-hash)])
    (λ new-formals
      (or (hash-ref! cache new-formals #f)
          (let ([result (apply lam new-formals)])
            (hash-set! cache new-formals result)
            result)))))

(define-syntax-rule (memoized-λ formals body ...)
  (let ([lam (λ formals body ...)])
    (memoize lam)))

;; DEFAULT PARAMETER VALUES ---------------------------------------------------

;; Path-String (List String String) -> (list String String)
(define (log-and-return kind outputs)
  (match-define (list stdout stderr) outputs)
  (when (verbose?)
    (displayln (~a ">>>>>>>>>>>>>>>>>>>>>> `" kind "` STDOUT"))
    (displayln stdout)
    (displayln (~a "---------------------- `"kind "` STDERR"))
    (displayln stderr)
    (displayln "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"))
  (list stdout stderr))

;; Path-String -> (list String String)
;; Runs module in file fpath in Racket interpreter and return
;; stdout and stderr produced
(define run-in-racket
  (memoized-λ (fpath)
    (let ([p-std-err (open-output-string)]
          [p-std-out (open-output-string)])
      (parameterize ([current-error-port p-std-err]
                     [current-output-port p-std-out]
                     [current-namespace (make-base-namespace)])
        (eval `(require (file ,fpath))))
      (list (get-output-string p-std-out)
            (get-output-string p-std-err)))))

;; Path-String -> (list String String)
;; Runs module in file fpath in Racket interpreter and return
;; stdout and stderr produced
(define (run-in-nodejs fpath)
  (match-define (list in-p-out out-p-in pid in-p-err control)
    (process* (build-path (output-directory) "node_modules" ".bin" "traceur")
              (module-output-file (if (absolute-path? fpath)
                                      (string->path fpath)
                                      (build-path (current-directory) fpath)))))
  (control 'wait)
  (list (port->string in-p-out)
        (port->string in-p-err)))

;; String String -> Boolean
;; Compare the outputs produced
(define (results-equal? racket js)
  (equal? racket js))

;; Path-String -> ExportTree
(define get-cached-export-tree
  (memoized-λ (test-fpath)
    (get-export-tree test-fpath)))

;; Path-String -> Void
;; Compile test-case in `fpath` to JavaScript
(define (compile-run-test-case fpath)
  (define test-path
    (if (absolute-path? fpath)
        (string->path fpath)
        (normalize-path (build-path (current-directory) fpath))))
  (parameterize ([main-source-file test-path]
                 [global-export-graph (get-cached-export-tree test-path)]
                 [current-source-file test-path]
                 [current-output-port (if (racketscript-stdout?)
                                          (current-output-port)
                                          (open-output-nowhere))])
    (racket->js)
    (list (log-and-return 'racket (run-in-racket fpath))
          (log-and-return 'nodejs (run-in-nodejs fpath)))))

;; Path-String -> Void
;; Rackunit check for RacketScript. Executes module at file fpath
;; in Racket and NodeJS and compare their outputs
(define-simple-check (check-racketscript fpath)
  (match-define (list (list r-p-out r-p-err)
                      (list j-p-out j-p-err))
    (compile-run-test-case fpath))
  (results-equal? r-p-out j-p-out))

;; -> Void
;; Initialize test environment.
;; 1. Build directory structure and install packages
;;    if necessary
;; 2. Always remove old compiled module outputs
;; 3. Always skip-npm-install to save time
(define (setup)
  (when (clean-output-before-test)
    (delete-directory/files (output-directory)))
  ;; clean the compiled modules always, to avoid
  ;; cases where compilation fails but it anyway
  ;; proceeds with last module output
  (let ([modules-directory (build-path (output-directory) "modules")])
    (when (directory-exists? modules-directory)
      (delete-directory/files modules-directory)))

  (prepare-build-directory "") ;; We don't care about bootstrap file
  (unless (skip-npm-install)
    (parameterize ([current-directory (output-directory)])
      (system "npm install")
      (skip-npm-install #t))))

;; (Listof Glob-Pattern) -> Void
;; If tc-search-patterns is simply a path to directory, run all test
;; cases in that directory otherwise use glob pattern
(define (run-tests tc-search-patterns)
  (define testcases
    (append-map (λ (pattern)
                  (if (string-suffix? pattern ".rkt")
                      (glob pattern)
                      (glob (~a pattern "/*.rkt"))))
                tc-search-patterns))

  (define failed-tests '())


  ;; Handler when exception is raised by check failures. Gather
  ;; all failed tests, and in verbose mode show check failure
  ;; message.
  (current-check-handler
   (let ([original-handler (current-check-handler)])
     (λ (t)
       (set! failed-tests (cons (current-test-name) failed-tests))
       (when (verbose?)
         ;; Show check failure result
         (original-handler t)))))

  (current-check-around
   (let ([original-check-around (current-check-around)])
     (λ (test-thunk)
       (with-handlers ([exn:test? (λ (e)
                                    (displayln "✘")
                                    ((current-check-handler) e)
                                    #f)]
                       [(thunk* #t) (λ (e)
                                      (displayln "✘ [𝚌𝚛𝚊𝚜𝚑]")
                                      ((current-check-handler) e)
                                      #f)])
         (let ([result (test-thunk)])
           (displayln (if result "✔" "✘")))))))

  (for ([test testcases]
        [i (in-naturals 1)])
    (define test-rel-path (find-relative-path (current-directory) test))

    (display (format "TEST (~a/~a) => ~a " i (length testcases) test-rel-path))
    (flush-output)

    (parameterize ([current-test-name test])
      (check-racketscript test))

    ;; Disable Gulp build as soon as we have run it once, as all we
    ;; need is HAMT built in runtime.1
    (unless (skip-gulp-build)
      (skip-gulp-build #t)))

  (unless (empty? failed-tests)
    (displayln (format "\nFailed tests (~a/~a) => "
                       (length failed-tests)
                       (length testcases)))
    (for ([t failed-tests])
      (displayln (format "  ✘ ~a" t)))))

;; Runs tests with each kind of option
(define (run tc-search-patterns)
  (setup)

  (displayln "-> RacketScript Fixtures Runner <-\n")
  (parameterize ([enabled-optimizations (set)])
    (displayln "---------------------------------")
    (displayln "::: Optimizations on ::: none :::")
    (displayln "---------------------------------")
    (run-tests tc-search-patterns))

  (displayln "")
  (parameterize ([enabled-optimizations (set self-tail->loop)])
    (displayln "--------------------------------")
    (displayln "::: Optimizations on ::: TCO :::")
    (displayln "--------------------------------")
    (run-tests tc-search-patterns))

  (displayln "")
  (parameterize ([enabled-optimizations (set flatten-if-else)])
    (displayln "--------------------------------------------")
    (displayln "::: Optimizations on ::: Flatten If-Else :::")
    (displayln "-------------------------------------------")
    (run-tests tc-search-patterns))

  (displayln "")
  (parameterize ([enabled-optimizations (set flatten-if-else
                                             self-tail->loop)])
    (displayln "--------------------------------")
    (displayln "::: Optimizations on ::: All :::")
    (displayln "-------------------------------")
    (run-tests tc-search-patterns)))

(skip-npm-install #f) ;; For setup we need to install packages
(module+ main
  ;; For setup we keep this on by default, and later turned off

  (define tc-search-pattern
    (command-line
     #:program "racketscript-fixture"
     #:usage-help "Run RacketScript test programs and compare against Racket"
     #:once-each
     [("-c" "--clean") "Clean previous build directory and reinstall packages"
      (clean-output-before-test #t)]
     [("-o" "--compiler-out") "Show RacketScript output"
      (racketscript-stdout? #t)]
     [("-n" "--skip-npm") "Skip NPM install on setup"
      (skip-npm-install #t)]
     [("-v" "--verbose") "Show exceptions when running tests."
      (racketscript-stdout? #t)
      (verbose? #t)]
     #:args (p . ps*)
     (cons p ps*)))

  (run tc-search-pattern))

(module+ test
  (define-runtime-path fixture-module "fixture.rkt")
  (define fixture-module-dir (path-only fixture-module))

  (define (fixture-path-patterns . paths)
    (map (λ (p)
           (~a (build-path fixture-module-dir p) "/*.rkt"))
         paths))

  (run (fixture-path-patterns "test-the-test"
                              "basic"
                              "struct"
                              "hash"
                              "wcm")))
