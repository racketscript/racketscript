#lang racket/base

(#%require (rename '#%unsafe i+ unsafe-fx+)
           (rename '#%unsafe i- unsafe-fx-))

(displayln (i+ 1 1))
(displayln (i- 1 1))
(displayln (i+ 2 8))
(displayln (i- 12 2))
