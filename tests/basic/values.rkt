#lang racket

(displayln "Hello World")
(displayln "Hello\n World")
(displayln "\"Hello World\"")
(displayln "'Hello World'")
(displayln #"Hello World")
;; TODO
;(displayln #rx"Hello World")
;(displayln #px"Hello World")
;(displayln #rx#"Hello World")
;(displayln #px#"Hello World")

(displayln 10)
(displayln 10.10)

(displayln 'hello)
(displayln 'hello-world)
(displayln 'hello-world+)

(displayln '(1 . 2))
(displayln '(1 2 3 4 5 . 2))

(displayln '(1 2 3 4 5))
(displayln '(1 2 "string" 4 5))
(displayln '(1 2 ("string" "in" ("quotes" "!")) 4 5))

;; TODO
;(displayln '(#"rx")) ; currently prints ([object Uint8Array])
