#lang racket

(define str "Hello🎂 World")

(displayln str)
(displayln (immutable? str))

(displayln (string-ref str 5))
(displayln (string-upcase str))
(displayln (immutable? (string-upcase str)))
(displayln (string-downcase str))
(displayln (immutable? (string-downcase str)))

(displayln (string=? "Hello🎂 World" str))
(displayln (string<? "ﬆ" "𝌆"))
(displayln (string<=? "ﬆ" "𝌆"))

(displayln (make-string 5 #\c))
(displayln (make-string 5 #\🎂))
(displayln (immutable? (make-string 3 #\a)))

(displayln (string #\a #\🎂 #\c))
(displayln (immutable? (string #\a #\🎂 #\c)))
(displayln (string->immutable-string (string #\a #\🎂 #\c)))
(displayln (immutable? (string->immutable-string (string #\i #\🎂 #\c))))

(displayln (list->string '(#\a #\🎂 #\c)))
(displayln (immutable? (list->string '(#\a #\🎂 #\c))))
(displayln (string-length (list->string '(#\a #\🎂 #\c))))

(displayln (substring str 3))
(displayln (substring str 3 7))
(displayln (immutable? (substring str 3)))

(displayln (immutable? "hi"))
(displayln (immutable? (string-upcase "hi")))

(displayln (string-append "Hello" "World" "🎂"))
(displayln (immutable? (string-append "a" "🎂")))

; Valid string->number arguments
(displayln (string->number "123"))
(displayln (string->number "-123"))
(displayln (string->number "A"))
(displayln (string->number "AFe1" 16))
(displayln (string->number "1101" 2))

; Invalid string->number arguments
(displayln (string->number " 123"))
(displayln (string->number "123x"))
(displayln (string->number "2" 2))
(displayln (string->number "" 2))
(displayln (string->number " "))

;; These don't work because RacketScript cannot yet handle racket/string.
; (displayln (car (string-split "Hello+World" "+")))
; (displayln (immutable? (car (string-split "Hello+World" "+"))))
; (displayln (car (cdr (string-split "Hello+World" "+"))))

;; Mutable string methods

(define mstr (string #\a #\🎂 #\c))
(string-set! mstr 2 #\x)
(displayln mstr)

;; string cmp
(string<=? "b" "aa")
(string<=? "aa" "b")
(string<=? "aa" "a")
(string<=? "a" "aa")
(string<=? "aa" "aa")

(string<? "b" "aa")
(string<? "aa" "b")
(string<? "aa" "a")
(string<? "a" "aa")
(string<? "aa" "aa")

(string>=? "b" "aa")
(string>=? "aa" "b")
(string>=? "aa" "a")
(string>=? "a" "aa")
(string>=? "aa" "aa")

(string>? "b" "aa")
(string>? "aa" "b")
(string>? "aa" "a")
(string>? "a" "aa")
(string>? "aa" "aa")

;; test printing of fn name
string
string?
string-append
