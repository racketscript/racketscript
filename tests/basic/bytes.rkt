#lang racket
(displayln (bytes->string/utf-8 #"Hello World"))
(displayln (string->bytes/utf-8 "Hello World"))
