#lang racket

(require (rename-in "private/simple-provide.rkt"
                    [say-hello say-hi]
                    [say-ahoy say-hi2]))

(say-hi "World!")
(say-hi2 "World!")
