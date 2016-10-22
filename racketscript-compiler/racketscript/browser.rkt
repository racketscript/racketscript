#lang racketscript/base

(provide (rename-out [-window window])
         document
         console)

(define -window ($ 'window))
(define document ($ -window 'document))
(define console ($ -window 'console))
