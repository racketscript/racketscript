#lang racket/base

(define v #\u5c)
(displayln v)

(displayln (char? #\a))
(displayln (char? 1))
(displayln (char? '(#\a)))

; TODO: This test fails because we do not yet have a separate type for Char.
; (displayln (char? "x"))

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

(displayln (char->integer #\a))
(displayln (char->integer #\☺))
