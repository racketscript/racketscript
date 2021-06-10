#lang racket
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

;; NOTE: Racket bytes are raw js Uint8Array;
;; printed output won't match Racket, so convert to string here
(bytes->string/utf-8 (make-bytes 5 65))
(bytes->string/utf-8 (make-bytes 5 0))
(bytes->string/utf-8 (make-bytes 5))
(bytes->string/utf-8 (make-bytes 0))
(bytes->string/utf-8 (make-bytes 0 0))

(bytes-ref (make-bytes 5 65) 1)

(define bs (make-bytes 5 65))
(bytes-set! bs 0 66)
(bytes-ref bs 0)
