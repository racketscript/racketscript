#lang racket/base

(displayln (format "~x" 1))
(displayln (format "~x" 50))
(displayln (format "~b" 37))
(displayln (format "~o" 37))

(displayln (format "~~"))
(displayln (format "~n"))
(displayln (format "~%"))

(displayln (format "~    hello"))

(displayln (format "~a" (void)))
(displayln (format "~s" (void)))
(displayln (format "~v" (void)))

(displayln (format "~a" 'apple))
(displayln (format "~s" 'apple))
(displayln (format "~v" 'apple))

(displayln (format "~a" '#:apple))
(displayln (format "~s" '#:apple))
(displayln (format "~v" '#:apple))

(displayln (format "~a" #f))
(displayln (format "~s" #f))
(displayln (format "~v" #f))
(displayln (format "~a" #t))
(displayln (format "~s" #t))
(displayln (format "~v" #t))

(displayln (format "~a" 135))
(displayln (format "~s" 135))
(displayln (format "~v" 135))
(displayln (format "~b" 135))
(displayln (format "~o" 135))
(displayln (format "~x" 135))

(displayln (format "~a" #\b))
(displayln (format "~s" #\b))
(displayln (format "~v" #\b))
(displayln (format "~a" #\tab))
(displayln (format "~s" #\tab))
(displayln (format "~v" #\tab))

(displayln (format "~a" "hello\tworld"))
(displayln (format "~s" "hello\tworld"))
(displayln (format "~v" "hello\tworld"))

(displayln (format "~a" (box "apple\tis a fruit")))
(displayln (format "~s" (box "apple\tis a fruit")))
(displayln (format "~v" (box "apple\tis a fruit")))

(displayln (format "~a" '(135 #\b "hello\tworld" (335 #\tab "hello\tworld"))))
(displayln (format "~s" '(135 #\b "hello\tworld" (335 #\tab "hello\tworld"))))
(displayln (format "~v" '(135 #\b "hello\tworld" (335 #\tab "hello\tworld"))))

; We are only checking the length of the output for hash,
; because the order is not guaranteed (and in fact different).
(displayln (string-length (format "~a" (hash "apple" 'red "banana" 'yellow))))
(displayln (string-length (format "~s" (hash "apple" 'red "banana" 'yellow))))
(displayln (string-length (format "~v" (hash "apple" 'red "banana" 'yellow))))

(displayln (format "~a" (vector "a" "b" "c")))
(displayln (format "~s" (vector "a" "b" "c")))
(displayln (format "~v" (vector "a" "b" "c")))

(displayln (format "~a" 'apple))
(displayln (format "~s" 'apple))
(displayln (format "~v" 'apple))

(define fn (λ () (void)))
(displayln (format "~a" fn))
(displayln (format "~s" fn))
(displayln (format "~v" fn))

(displayln (format "~a is ~x in hex" "15" 15))
