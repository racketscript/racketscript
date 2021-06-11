#lang racket

(require "../test-utils.rkt")

(displayln (bytes->string/utf-8 #"Hello World"))
(displayln (string->bytes/utf-8 "Hello World"))
(displayln (bytes=? #"abc" #"abc"))
(displayln (bytes=? #"abc" #"abd"))
(bytes? (void))

;; TODO: Not implemented correctly yet.
; (writeln #"abc")
; (println #"abc")

;; cmp
(bytes<? #"b" #"aa")
(bytes<? #"aa" #"b")
(bytes<? #"aa" #"a")
(bytes<? #"a" #"aa")
(bytes<? #"aa" #"aa")

(bytes>? #"b" #"aa")
(bytes>? #"aa" #"b")
(bytes>? #"aa" #"a")
(bytes>? #"a" #"aa")
(bytes>? #"aa" #"aa")

(make-bytes 5 65)
;; TODO: 0 byte doesnt print the same as Racket
(bytes->string/utf-8 (make-bytes 5 0))
(bytes->string/utf-8 (make-bytes 5))
(make-bytes 0)
(make-bytes 0 0)

(bytes-ref (make-bytes 5 65) 1)

(define bs (make-bytes 5 65))
(bytes-set! bs 0 66)
(bytes-ref bs 0)
(err/rt-test (bytes-ref bs 6)) ;; out of range
(err/rt-test (bytes-set! bs 6 66)) ;; out of range
