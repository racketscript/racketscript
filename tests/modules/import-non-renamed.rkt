#lang racket

(require (rename-in "private/simple-provide.rkt"
                    [say-hello say-hola]))

(say-hola "World!")
