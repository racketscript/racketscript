#lang racket/base

(define o-str-port (open-output-string))
(displayln (port? o-str-port))
(displayln (output-port? o-str-port))
(display "Hello" o-str-port)
(displayln (get-output-string o-str-port))
(display " " o-str-port)
(display "World" o-str-port)
(displayln (get-output-string o-str-port))

(displayln (port? (current-output-port)))
(displayln (output-port? (current-output-port)))
(display "Test\n" (current-output-port))
