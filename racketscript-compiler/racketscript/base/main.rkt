#lang racket/base

(require "../interop.rkt")

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [#%plain-module-begin #%module-begin]))
(provide (all-from-out "../interop.rkt"))
