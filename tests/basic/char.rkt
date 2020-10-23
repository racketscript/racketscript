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
(println #\🎂)

(println (char-upcase #\a))
(println (char-upcase #\ß))
(println (char-upcase #\ﬁ))  ; upper case is longer than 1 character (FI).
(println (char-downcase #\A))
(println (char-downcase #\ẞ))

;; Not implemented:
; (println (char-titlecase #\a))
; (println (char-foldcase #\A))
; (println (char-foldcase #\ß))
; (println (char-foldcase #\ß))

(displayln "char-alphabetic-case?")
(println (char-alphabetic? #\A))
(println (char-alphabetic? #\Я))
(println (char-alphabetic? #\tab))

(displayln "char-lower-case?")
(println (char-lower-case? #\A))
(println (char-lower-case? #\a))
(println (char-lower-case? #\Я))
(println (char-lower-case? #\я))
(println (char-lower-case? #\5))

(displayln "char-upper-case?")
(println (char-upper-case? #\A))
(println (char-upper-case? #\a))
(println (char-upper-case? #\Я))
(println (char-upper-case? #\Я))
(println (char-upper-case? #\5))

(displayln "char-title-case?")
(println (char-title-case? #\A))
(println (char-title-case? #\a))
(println (char-title-case? #\Я))
(println (char-title-case? #\Я))
(println (char-title-case? #\5))

(displayln "char-numeric?")
(println (char-numeric? #\5))  ; Nd
(println (char-numeric? #\Ⅻ))  ; Nl
(println (char-numeric? #\㊲))  ; No
;; TODO: the following should return #t but returns #f
;; see comments in runtime/core/char.js
;; and https://github.com/vishesh/racketscript/issues/176
;; (println (char-numeric? #\六))  ; Lo

(displayln "char-symbolic?")
(println (char-symbolic? #\A))
(println (char-symbolic? #\∄))

(displayln "char-punctuation?")
(println (char-punctuation? #\A))
(println (char-punctuation? #\;))

(displayln "char-graphic?")
(println (char-graphic? #\A))
(println (char-graphic? #\🎂))
(println (char-graphic? #\㊲))
(println (char-graphic? #\tab))

(displayln "char-whitespace?")
(println (char-whitespace? #\A))
(println (char-whitespace? #\space))
(println (char-whitespace? #\newline))
(println (char-whitespace? #\tab))

(displayln "char-blank?")
(println (char-blank? #\A))
(println (char-blank? #\space))
(println (char-blank? #\newline))
(println (char-blank? #\tab))

(displayln "char-iso-control?")
(println (char-iso-control? #\A))
(println (char-iso-control? #\nul))
(println (char-iso-control? #\rubout))
(println (char-iso-control? #\u9F))
