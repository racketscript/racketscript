#lang racket

(displayln 'foobar)
(displayln (symbol-interned? 'foobar))

(displayln (string->symbol "foobar"))
(displayln (eq? (string->symbol "foobar") 'foobar))
(displayln (eq? (string->uninterned-symbol "foobar") 'foobar))
(displayln (symbol-interned? (string->uninterned-symbol "foobar")))

(displayln (symbol-interned? (gensym 'foo)))
