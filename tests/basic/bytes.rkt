#lang racket
(displayln (bytes->string/utf-8 #"Hello World"))
(displayln (string->bytes/utf-8 "Hello World"))
(displayln (bytes=? #"abc" #"abc"))
(displayln (bytes=? #"abc" #"abd"))
(bytes? (void))

;; TODO: Not imlemented correctly yet.
; (writeln #"abc")
; (println #"abc")
