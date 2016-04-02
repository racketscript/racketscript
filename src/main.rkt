#lang racket/base

(require racket/cmdline
         threading
         "absyn.rkt"
         "expand.rkt"
         "analyze.rkt"
         "transform.rkt"
         "assembler.rkt")

(define (racket->js filename)
  (~> (quick-expand filename)
      (rename-program _)
      (absyn-top-level->il _)
      (assemble _)))

(module+ main
  (define source
    (command-line
     #:args (filename) filename))

  (racket->js source))
