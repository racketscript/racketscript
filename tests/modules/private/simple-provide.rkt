#lang racket

(provide say-hello
         (rename-out [say-hola say-ahoy]))

(define (say-hello msg)
  (displayln (list "Hello" msg)))

(define (say-hola msg)
  (displayln (list "Hola" msg)))
