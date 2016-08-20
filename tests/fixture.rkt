#!/usr/bin/env racket
#lang racket

(require rackunit
         glob
         "../src/global.rkt"
         "../src/main.rkt"
         "../src/moddeps.rkt")

(define tests-root-dir (build-path rapture-dir "tests"))

;; Print Racket and JS output of test programs to stdout
;; Also show check failure
(define verbose? (make-parameter #f))

;; Do a complete cleanup for previous build directory before starting
;; tests.
(define clean-output-before-test (make-parameter #f))

;; Turning if false would ignore all standard output
;; produced by compiler
(define rapture-stdout? (make-parameter #f))

;; DEFAULT PARAMETER VALUES ---------------------------------------------------

;; Path-String Input-Port Input-Port -> (list String String)
(define (log-and-return fpath kind in-p-out in-p-err)
  ;; TODO: Log outputs
  (let ([p-out (port->string in-p-out)]
        [p-err (port->string in-p-err)])
    (when (verbose?)
      (displayln (~a ">>>>>>>>>>>>>>>>>>>>>> `" kind "` STDOUT"))
      (displayln p-out)
      (displayln (~a "---------------------- `"kind "` STDERR"))
      (displayln p-err)
      (displayln "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"))
    (list p-out p-err)))

;; Path-String -> (list String String)
;; Runs module in file fpath in Racket interpreter and return
;; stdout and stderr produced
(define (run-in-racket fpath)
  (match-define (list in-p-out out-p-in pid in-p-err control)
    (process* (find-executable-path "racket")
              (~a fpath)))
  (control 'wait)
  (log-and-return fpath 'racket in-p-out in-p-err))

;; Path-String -> (list String String)
;; Runs module in file fpath in Racket interpreter and return
;; stdout and stderr produced
(define (run-in-nodejs fpath)
  (match-define (list in-p-out out-p-in pid in-p-err control)
    (process* (find-executable-path "node")
              (build-path (output-directory) "bootstrap.js")))
  (control 'wait)
  (log-and-return fpath 'nodejs in-p-out in-p-err))

;; String String -> Boolean
;; Compare the outputs produced
(define (results-equal? racket js)
  (equal? racket js))

;; Path-String -> Void
;; Rackunit check for Rapture. Executes module at file fpath
;; in Racket and NodeJS and compare their outputs
(define-simple-check (check-rapture fpath)
  ;; First compile to JS
  (define compile-result
    (let ([test-path (build-path tests-root-dir fpath)])
      (parameterize ([main-source-file test-path]
                     [global-export-graph (get-export-tree test-path)]
                     [current-source-file test-path]
                     [current-output-port (if (rapture-stdout?)
                                              (current-output-port)
                                              (open-output-nowhere))])
        (skip-gulp-build #f) ;;TODO: Remove this to speed up
        (racket->js)
        #t)))

  (cond
    [(false? compile-result) #f]
    [else
     (match-define (list r-p-out r-p-err) (run-in-racket fpath))
     (match-define (list j-p-out j-p-err) (run-in-nodejs fpath))
     (if (results-equal? r-p-out j-p-out)
         (begin (displayln "✔") #t)
         (begin (displayln "✘") #f))]))

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
  (for ([f (glob (~a (output-directory) "/modules" "/*"))])
    (delete-file f))

  (prepare-build-directory "") ;; We don't care about bootstrap file
  (unless (skip-npm-install)
    (parameterize ([current-directory (output-directory)])
      (system "npm install")
      (skip-npm-install #t))))

;; (Listof Glob-Pattern) -> Void
;; If tc-search-patterns is simply a path to directory, run all test
;; cases in that directory otherwise use glob pattern
(define (run-tests tc-search-patterns)
  (setup)

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


  (for ([test testcases]
        [i (in-naturals 1)])
    (display (format "TEST (~a/~a) => ~a " i (length testcases) test))
    (parameterize ([current-test-name test])
      (check-rapture test)))

  (unless (empty? failed-tests)
    (displayln (format "\nFailed tests (~a/~a) => "
                       (length failed-tests)
                       (length testcases)))
    (for ([t failed-tests])
      (displayln (format "  ✘ ~a" t)))))


(skip-npm-install #f) ;; For setup we need to install packages
(module+ main
  ;; For setup we keep this on by default, and later turned off

  (define tc-search-pattern
    (command-line
     #:program "rapture-fixture"
     #:usage-help "Run Rapture test programs and compare against Racket"
     #:once-each
     [("-c" "--clean") "Clean previous build directory and reinstall packages"
      (clean-output-before-test #t)]
     [("-o" "--rapture-out") "Show rapture output"
      (rapture-stdout? #t)]
     [("-n" "--skip-npm") "Skip NPM install on setup"
      (skip-npm-install #t)]
     [("-v" "--verbose") "Show exceptions when running tests."
      (rapture-stdout? #t)
      (verbose? #t)]
     #:args (pattern)
     pattern))

  (run-tests (list tc-search-pattern)))

(module+ test
  (run-tests (list "basic/*.rkt"
                   "struct/*.rkt"
                   "hash/*.rkt")))
