#lang racket/base

(define v #\u5c)
(displayln v)

(displayln (char? #\a))
(displayln (char? 1))
(displayln (char? '(#\a)))

(displayln (char? "x"))

(displayln (char<? #\a #\b))
(displayln (char<? #\a #\a))
(displayln (char<? #\b #\a))

; This will produce the wrong result in JavaScript if the implementation
; compares characters directly instead of looking at their codepoints.
(displayln (char<? #\ﬆ #\𝌆))

(displayln (char<=? #\a #\a))
(displayln (char<=? #\a #\b))
(displayln (char<=? #\b #\a))

(displayln (char>? #\a #\a))
(displayln (char>? #\a #\b))
(displayln (char>? #\b #\a))

(displayln (char>=? #\a #\a))
(displayln (char>=? #\a #\b))
(displayln (char>=? #\b #\a))

(displayln (char=? #\a #\a))
(displayln (char=? #\b #\a))
(displayln (char=? #\u10 #\newline))

(displayln (char->integer #\a))
(displayln (char->integer #\☺))

(displayln (integer->char 127874))

(displayln (char-utf-8-length #\a))
(displayln (char-utf-8-length #\🎂))

(println #\a)
(println #\tab)
(println #\߆)

; This test requires a full implementation of isGraphicCodepoint to pass.
; (println #\🎂)
