#lang racket

(require rackunit
         glob
         "../src/main.rkt")

(define clean-output-before-test (make-parameter #f))
(define tests-root-dir (build-path rapture-dir "tests"))
(define rapture-stdout? (make-parameter #f))

(define (log-and-return fpath in-p-out in-p-err)
  ;; TODO: Log outputs
  (list (port->string in-p-out)
        (port->string in-p-err)))

(define (run-in-racket fpath)
  (match-define (list in-p-out out-p-in pid in-p-err control)
    (process* "/usr/bin/racket"
              (~a fpath)))
  (control 'wait)
  (log-and-return fpath in-p-out in-p-err))

(define (results-equal? racket js)
  (equal? racket js))

(define (run-in-nodejs fpath)
  (match-define (list in-p-out out-p-in pid in-p-err control)
    (process* "/usr/bin/node" ;; TODO: Get this from $PATH
              (build-path (output-directory) "bootstrap.js")))
  (control 'wait)
  (log-and-return fpath in-p-out in-p-err))

(define-simple-check (check-rapture fpath)
  ;; First compile to JS
  (define compile-result
    (let ([test-path (build-path tests-root-dir fpath)])
      (parameterize ([main-source-file test-path]
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
     (results-equal? r-p-out j-p-out)]))

(define (setup)
  (when (clean-output-before-test)
    (delete-directory/files (output-directory)))
  ;; clean the compiled modules always, to avoid
  ;; cases where compilation fails but it anyway
  ;; proceeds with last module output
  (for ([f (glob (~a (output-directory) "/modules" "/*"))])
    (delete-file f))

  (skip-npm-install #t)
  (skip-gulp-build  #t)

  (prepare-build-directory "") ;; We don't care about bootstrap file
  (parameterize ([current-directory (output-directory)])
    (system "npm install")))

(define (run-tests tc-search-pattern)
  (setup)

  (define testcases
    (if (string-suffix? tc-search-pattern ".rkt")
        (glob tc-search-pattern)
        (glob (~a tc-search-pattern "/*.rkt"))))

  (for ([test testcases]
        [i (in-naturals 1)])
    (displayln (format "\nTEST (~a/~a) => ~a " i (length testcases) test))
    (check-rapture test)))

(module+ main
  (define tc-search-pattern
    (command-line
     #:program "rapture-fixture"
     #:usage-help "Run Rapture test programs"
     #:once-each
     ["--clean" "Clean previous build directory and reinstall packages"
      (clean-output-before-test #t)]
     ["--rapture-out" "Show rapture output"
      (rapture-stdout? #t)]
     #:args (pattern)
     pattern))
  
  (run-tests tc-search-pattern))

(module+ test
  (run-tests "basic/*.rkt"))
