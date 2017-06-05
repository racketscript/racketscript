#lang racket

(provide (rename-out [say-hola say-hello])
         (rename-out [say-hello say-hola]))


(define (say-hello msg)
  (displayln (list "Hello" msg)))

(define (say-hola msg)
  (displayln (list "Hola" msg)))
