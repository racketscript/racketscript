#lang racket/base

(require racket/bool
         racket/cmdline
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
         racket/serialize
         syntax/moddep

         data/queue
         threading

         "absyn.rkt"
         "il.rkt"
         "il-analyze.rkt"
         "assembler.rkt"
         "config.rkt"
         "expand.rkt"
         "global.rkt"
         "logging.rkt"
         "moddeps.rkt"
         "transform.rkt"
         "util.rkt")

(provide current-source-file
         main-source-file
         output-directory
         prepare-build-directory
         racket->js
         racketscript-dir
         skip-gulp-build
         skip-npm-install
         enabled-optimizations
         recompile-all-modules?)

(define build-mode (make-parameter 'complete))
(define skip-npm-install (make-parameter #f))
(define skip-gulp-build (make-parameter #f))
(define js-output-file (make-parameter "compiled.js"))
(define js-output-beautify? (make-parameter #f))
(define enabled-optimizations (make-parameter (set)))
(define input-from-stdin? (make-parameter #f))
(define recompile-all-modules? (make-parameter #f))

(define *js-bootstrap-file* "bootstrap.js")
(define *browser-index-file* "index.html")

;; Compiler for ES6 to ES5 compilation.
;; - "babel"
;; - "traceur"
;; - "webpack" ;;TODO
(define *targets* (list "traceur"
                        "traceur-browser"
                        "babel"
                        "babel-webpack"
                        "closure-compiler"))
(define js-target (make-parameter "traceur"))

;; Path-String -> Path
;; Return path of support file named f
(define (support-file f)
  (build-path racketscript-dir "compiler" "js-support" (js-target) f))

;; PathString -> Path
;; Return path of runtime file named f
(define (runtime-file f)
  (build-path racketscript-dir "compiler" "runtime" f))

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
  (copy-directory (build-path racketscript-dir "compiler" "runtime")
                  (output-directory)))

;; -> Void
(define (copy-support-files)
  (match (js-target)
    ["traceur"
     (copy-file+ (support-file *js-bootstrap-file*)
                 (output-directory))]
    ["traceur-browser"
     (copy-file+ (support-file *browser-index-file*)
                 (output-directory))]
    [_ (void)]))

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
      (system (~a "./"
                  (build-path "node_modules"
                              ".bin"
                              "gulp"))))))

;;;; Generate stub module

;; Module -> ILModule
;; Applies the enabled optimization to translated Absyn
(define (absyn-module->il* ast)
  (define il (absyn-module->il ast))
  (define (apply-optimizations il)
    (for/fold ([il il])
              ([opt (in-set (enabled-optimizations))])
      (il-apply-optimization il opt)))
  (converge apply-optimizations il))

;; Path-String -> (Listof Symbol)
;; Read the runtime JavaScript file which, and find all the primitives
;; implemented there. Primitives are keys inside "exports" object.
(define (read-js-exports js-file)
  (call-with-input-file js-file
    (λ (in)
      (remove-duplicates
       (map (λ (s)
              (~> (bytes->string/utf-8 s)
                  (string-slice _ 9 -2)
                  (string->symbol _)))
            (regexp-match* #rx"exports\\[\"([^\"\\]|\\.)*\"\\]" in))))))

;;-----------------------------------------------------------------------------

(define *module-compile-timestamp-file* "timestamps.rktl")

(define (load-cached-module-timestamps)
  (define fpath (build-path (cache-directory) *module-compile-timestamp-file*))
  (if (file-exists? fpath)
      (with-handlers ([exn:fail? (λ (e)
                                   (log-rjs-info "invalid timestamp file")
                                   (make-hash))])
        (let ([ts (call-with-input-file fpath
                    (λ (in)
                      (deserialize (read in))))])
          (make-hash ts)))
      (make-hash)))

(define (dump-module-timestamps! ts)
  (define fpath (build-path (cache-directory) *module-compile-timestamp-file*))
  (call-with-output-file fpath #:exists 'truncate
    (λ (out)
      (write (serialize (hash->list ts)) out))))

(define (save-module-timestamp! ts mod)
  (hash-set! ts mod (file-or-directory-modify-seconds (actual-module-path mod))))

(define (get-module-timestamp ts mod)
  (hash-ref ts mod 0))

;; -> Void
;; Returns true if we can skip compilation of module `mod`. We check -
;;  - if parameter `recompile-all-modules?` is false
;;  - timestamp of module against its timestamp when we last compiled.
;;  - if the JavaScript output file exists
(define (skip-module-compile? ts mod)
  (and (not (recompile-all-modules?))
       (file-exists? (module-output-file mod))
       (= (get-module-timestamp ts mod)
          (file-or-directory-modify-seconds (actual-module-path mod)))))

;; -> Void
;; For given global parameters starts build process starting
;; with entry point module and all its dependencies
(define (racket->js)
  (define added (mutable-set))
  (define pending (make-queue))

  ;; build directories to output build folder.
  (define default-module-name (string-slice (~a (last-path-element
                                                 (main-source-file)))
                                            0 -4))
  (prepare-build-directory default-module-name)

  (define (put-to-pending! mod)
    (unless (set-member? added mod)
      (set-add! added mod)
      (enqueue! pending mod)))

  (define timestamps (load-cached-module-timestamps))

  (put-to-pending! (path->complete-path (main-source-file)))
  (for ([pm primitive-modules])
    (put-to-pending! pm))

  (let loop ()
    (define next (and (non-empty-queue? pending) (dequeue! pending)))
    (cond
      [(and next (skip-module-compile? timestamps next))
       (log-rjs-info (~a "Skipping " next))
       (loop)]
      [next
       (current-source-file next)
       (make-directory* (path-only (module-output-file next)))
       (save-module-timestamp! timestamps next)

       (define expanded (quick-expand next))
       (define ast (convert expanded (override-module-path next)))

       (assemble-module (insert-arity-checks
                         (absyn-module->il* ast))
                        #f)

       ;; Run JS beautifier
       (when (js-output-beautify?)
         (system (format "js-beautify -r ~a" (module-output-file next))))

       (for ([mod (in-set (Module-imports ast))])
         (match mod
           [(? symbol? _) (void)]
           [_ #:when (collects-module? mod) (void) (put-to-pending! mod)]
           [_ (put-to-pending! mod)]))
       (loop)]
      [(false? next)
       (dump-module-timestamps! timestamps)
       (log-rjs-info "Compiling ES6 to ES5.")
       (es6->es5)
       (log-rjs-info "Finished.")])))

;; String -> String
(define (js-string-beautify js-str)
  (match-define (list in-p-out out-p-in pid in-p-err control)
    (process* (~a (find-executable-path "js-beautify"))))
  (fprintf out-p-in js-str)
  (close-output-port out-p-in)
  (control 'wait)
  (port->string in-p-out))

(define (parse-command-line)
  (command-line
   #:program "racketscript"
   #:usage-help "Compile Racket to JavaScript"
   #:once-each
   [("-d" "--build-dir") dir "Output directory" (output-directory (simplify-path dir))]
   [("-n" "--skip-npm-install") "Skip NPM install phase" (skip-npm-install #t)]
   [("-g" "--skip-gulp-build") "Skip Gulp build phase" (skip-gulp-build #t)]
   [("-b" "--js-beautify") "Beautify JS output" (js-output-beautify? #t)]
   [("-r" "--force-recompile") "Re-compile all modules" (recompile-all-modules? #t)]
   ["--skip-arity-checks" "Skip arity checks in beginning of functions" (skip-arity-checks? #t)]
   ["--stdin" "Reads module from standard input, with file name argument being pseudo name"
    (input-from-stdin? #t)]
   ["--enable-self-tail" "Translate self tail calls to loops"
    (enabled-optimizations (set-add (enabled-optimizations) self-tail->loop))]
   ["--enable-flatten-if" "Flatten nested if-else statements"
    (enabled-optimizations (set-add (enabled-optimizations) flatten-if-else))]
   ["--lift-returns" "Translate self tail calls to loops"
    (enabled-optimizations (set-add (enabled-optimizations) lift-returns))]
   #:multi
   [("-t" "--target") target "ES6 to ES5 compiler [traceur|babel|traceur-browser|closure-compiler|babel-webpack]"
    (if (member target *targets*)
        (js-target target)
        (error "`~a` is not a supported target."))]
   #:once-any
   ["--expand" "Fully expand Racket source" (build-mode 'expand)]
   ["--ast" "Expand and print AST" (build-mode 'absyn)]
   ["--il" "Compile to intermediate langauge (IL)" (build-mode 'il)]
   ["--js" "Compile and print JS module to stdout" (build-mode 'js)]
   ["--complete" "Compile module and its dependencies to JS" (build-mode 'complete)]
   #:args ([filename 'stdin])
   (match `(,filename ,(input-from-stdin?))
     [`(,'stdin ,#t)
      (let ([complete-filename (build-path "/tmp/not-exist.rkt")])
        (current-source-file complete-filename)
        (main-source-file complete-filename)
        complete-filename)]
     [`(,'stdin ,#f) #f]
     [`(,_ ,#t) (error 'racketscript "Don't expect filename with `--stdin` mode")]
     [`(,filename ,#f)
      (let ([complete-filename (path->complete-path filename)])
        (current-source-file complete-filename)
        (main-source-file complete-filename)
        complete-filename)])))

(module+ main
  ;; Parse command line. If source is 'show-help, we call it again
  ;; with no arguments to show help. This is a little hack we had to
  ;; add for stdin compilation, as we don't expect any argument
  ;; (#:args) in that case but it matches anyway with no way of
  ;; falling back to help.
  (define source
    (match (parse-command-line)
      [#f (parameterize ([current-command-line-arguments #("--help")])
            (parse-command-line))]
      [v v]))

  ;; We can read module from stdin only when we wish to see different
  ;; IL or JS output on stdout. For complete compilation (or more
  ;; specifically non-debugging purposes), we would refuse to take
  ;; input from stdin.
  (when (and (input-from-stdin?)
             (equal? (build-mode) 'complete))
    (error 'racketscript "Can't compile with complete mode input from stdin"))

  (unless (equal? (build-mode) 'js)
    (log-rjs-info "RacketScript root directory: ~a" racketscript-dir))

  (unless (input-from-stdin?)
    ;; Initialize global-export-graph so that we can import each
    ;; module as an object and follow identifier's from there.
    ;; For stdin builds, we have to defer this operation.
    (unless (equal? (build-mode) 'js)
      ;; As 'js mode prints output to stdout, we don't want to mix
      (log-rjs-info "Resolving module dependencies and identifiers... "))
    (global-export-graph (get-export-tree source)))

  (define (expanded-module)
    (cond
      [(input-from-stdin?)
       ;; HACK: Just make an stupid guess that all that we will
       ;; ever use will come from standard library. Since we
       ;; need stdin from playground, its fine for now.
       ;; TODO: Figure out a way to compile this syntax to
       ;; module code bytecode
       (global-export-graph (get-export-tree (build-path racketscript-compiler-dir
                                                         "nothing.rkt")))
       (read-and-expand-module (current-input-port))]
      [else
       (quick-expand source)]))

  (match (build-mode)
    ['expand (~> (expanded-module)
                 (syntax->datum _)
                 (pretty-print _))]
    ['absyn (~> (expanded-module)
                (convert _ source)
                (pretty-print _))]
    ['il (~> (expanded-module)
             (convert _ source)
             (absyn-module->il* _)
             (insert-arity-checks _)
             (pretty-print _))]
    ['js
     (logging? #f)
     (define output-string (open-output-string))
     (~> (expanded-module)
         (convert _ source)
         (absyn-module->il* _)
         (insert-arity-checks _)
         (assemble-module _ output-string))
     (displayln
      (if (js-output-beautify?)
          (js-string-beautify (get-output-string output-string))
          (get-output-string output-string)))]
    ['complete (racket->js)])

  (void))
