#!/usr/bin/env racket
#lang racket

(require rackunit
         file/glob
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

;; Find path to NodeJS excecutable.
(define nodejs-executable-path (make-parameter (or (find-executable-path "node")
                                                   (find-executable-path "nodejs")
                                                   (error "NodeJS executable not found in PATH!"))))

;; Turning if false would ignore all standard output
;; produced by compiler
(define racketscript-stdout? (make-parameter #f))

;; For coverage mode, we just compile the test cases, but not the
;; examples, as we don't care about knowing result of tests.
(define coverage-mode? (let ([mod (getenv "COVERAGE_MODE")])
                         (and mod (equal? (string->number mod) 1))))

(define (displayln* v)
  (if coverage-mode?
      (displayln "")
      (displayln v)))

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
;; TODO: Handle error if we are unable to start process.
(define (run-in-nodejs fpath)
  (match-define (list in-p-out out-p-in pid in-p-err control)
    (process* (nodejs-executable-path)
              (module-output-file (if (absolute-path? fpath)
                                      (string->path fpath)
                                      (build-path (current-directory) fpath)))))
  (control 'wait)
  (define result (list (port->string in-p-out)
                       (port->string in-p-err)))

  (close-output-port out-p-in)
  (close-input-port in-p-out)
  (close-input-port in-p-err)

  result)


;; String String -> Boolean
;; Compare the outputs produced
(define (results-equal? racket js)
  (equal? racket js))

;; Path-String -> ExportTree
(define get-cached-export-tree
  (memoized-λ (test-fpath)
    (get-export-tree (list test-fpath))))

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
                 [recompile-all-modules? #f]
                 [current-output-port (if (racketscript-stdout?)
                                          (current-output-port)
                                          (open-output-nowhere))])
    (with-handlers ([exn:fail? (λ (e)
                                 (when (verbose?)
                                   ((error-display-handler) "racket->js failed" e))
                                 #f)])
      (racket->js)
      (if (false? coverage-mode?)
          (list (log-and-return 'racket (run-in-racket fpath))
                (log-and-return 'nodejs (run-in-nodejs fpath)))
          (list (list "" "")
                (list "" ""))))))

;; Path-String -> Void
;; Rackunit check for RacketScript. Executes module at file fpath
;; in Racket and NodeJS and compare their outputs
(define-simple-check (check-racketscript fpath)
  (let ([result (compile-run-test-case fpath)])
    (cond
      [(false? result)
       ;;FIXME
       ;;   We output here because if we return a non-false value
       ;;   from here, it doens't show up as a test failure. We
       ;;   ideally would like to handle this in check-around.
       (display "[𝚌𝚛𝚊𝚜𝚑]") result]
      [else (match-define (list (list r-p-out r-p-err)
                                (list j-p-out j-p-err))
              result)
            (results-equal? r-p-out j-p-out)])))

;; -> Void
;; Initialize test environment.
;; 1. Build directory structure and install packages
;;    if necessary
;; 2. Always skip-npm-install to save time
;; [3. Always remove old compiled module outputs)]
(define (setup)
  (skip-npm-install #t)

  (when (clean-output-before-test)
    (delete-directory/files (output-directory))))

;; (Listof Glob-Pattern) -> Boolean
;; If tc-search-patterns is simply a path to directory, run all test
;; cases in that directory otherwise use glob pattern.
;; Returns #t or #f depending on test results
(define (run-tests tc-search-patterns)
  ;; First clean the compiled modules always, to avoid cases where
  ;; compilation fails but it anyway proceeds with last module output
  (for ([dir '("modules" "cache")])
    (let ([p (build-path (output-directory) dir)])
      (when (directory-exists? p)
        (delete-directory/files p))))

  (define skipped-tests (mutable-set))

  ;; skip-test? : String Path -> Bool
  ;; Filter tests whose names start with "__"
  (define (skip-test? path)
    (define skip?
      (string-prefix? (last (string-split path "/")) "__"))
    (when skip? (set-add! skipped-tests path))
    skip?)

  (define testcases ; rest of fixture assumes list of string paths
    (filter-not
     skip-test?
     (map
      path->string ; file/glob returns paths not strings, but rest of fixture expects strs
      (apply
       append
       (for/list ([pat tc-search-patterns])
         ; test all rkt files in dir, unless given single file
         (if (string-suffix? pat ".rkt")
             (glob pat)
             (glob (~a pat "/*.rkt"))))))))

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
       (with-handlers ([exn:test:check? (λ (e)
                                          (displayln* "✘")
                                          ((current-check-handler) e))])
         (test-thunk)
         (displayln* "✔")))))

  (for ([test testcases]
        [i (in-naturals 1)])
    (define test-rel-path (find-relative-path (current-directory) test))

    (display (format "TEST (~a/~a) => ~a " i (length testcases) test-rel-path))
    (flush-output)

    (parameterize ([current-test-name test])
      (check-racketscript test)))

  (unless (empty? failed-tests)
    (displayln (format "\nFailed tests (~a/~a) => "
                       (length failed-tests)
                       (length testcases)))
    (for ([t failed-tests])
      (displayln (format "  ✘ ~a" t))))

  (unless (set-empty? skipped-tests)
    (displayln (format "\nSkipped tests [~a] => " (set-count skipped-tests)))
    (for ([t (sort (set->list skipped-tests) string<?)])
      (displayln (format "  □ ~a" t))))

  (empty? failed-tests))


;; Runs tests with each kind of option
(define (run tc-search-patterns)
  (setup)
  (define passed #t)
  (define (set-passed! new-value)
    (set! passed (and passed new-value)))

  (displayln "-> RacketScript Fixtures Runner <-\n")
  (when coverage-mode? (displayln "Running in coverage mode."))

  (unless coverage-mode?
    (parameterize ([enabled-optimizations (set)])
      (displayln "---------------------------------")
      (displayln "::: Optimizations on ::: none :::")
      (displayln "---------------------------------")
      (set-passed! (run-tests (filter-not
                      (λ (s) (string-contains? s "optimize"))
                      tc-search-patterns)))))

  ;; (displayln "")
  ;; (parameterize ([enabled-optimizations (set self-tail->loop)])
  ;;   (displayln "--------------------------------")
  ;;   (displayln "::: Optimizations on ::: TCO :::")
  ;;   (displayln "--------------------------------")
  ;;   (run-tests tc-search-patterns))

  ;; (displayln "")
  ;; (parameterize ([enabled-optimizations (set flatten-if-else)])
  ;;   (displayln "--------------------------------------------")
  ;;   (displayln "::: Optimizations on ::: Flatten If-Else :::")
  ;;   (displayln "-------------------------------------------")
  ;;   (run-tests tc-search-patterns))

  (displayln "")
  (parameterize ([enabled-optimizations (set flatten-if-else
                                             self-tail->loop)])
    (displayln "--------------------------------")
    (displayln "::: Optimizations on ::: All :::")
    (displayln "-------------------------------")
    (set-passed! (run-tests tc-search-patterns)))

  (unless passed (exit 1)))

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
    (for/list ([p paths]) (~a (build-path fixture-module-dir p) "/*.rkt")))

  (run (fixture-path-patterns "racket-core"
                              "test-the-test"
                              "basic"
                              "struct"
                              "hash"
                              "wcm"
                              "modules"
                              "optimize"
                              "experimental")))
