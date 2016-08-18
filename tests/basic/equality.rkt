#lang racket

(displayln "unequal numbers")
(equal? 1 2)
(eqv? 1 2)
(eq? 1 2)

(displayln "equal numbers")
(equal? 1 1)
(eqv? 1 1)
(eq? 1 1)

(displayln "unequal string")
(equal? "v" "a")
(eqv? "v" "a")
(eq? "v" "a")

(displayln "equal string")
(equal? "v" "v")
(eqv? "v" "v")
(eq? "v" "v")

(displayln "unequal symbol")
(equal? 'foo 'bar)
(eqv? 'foo 'bar)
(eq? 'foo 'bar)

(displayln "equal symbol")
(equal? 'foo 'foo)
(eqv? 'foo 'foo)
(eq? 'foo 'foo)

(struct posn (x y) #:transparent)

(displayln "unequal struct")
(equal? (posn 1 2) (posn 1 2))
(eqv? (posn 1 2) (posn 1 2))
(eq? (posn 1 2) (posn 1 2))

(displayln "equal struct")
(equal? (posn 1 1) (posn 1 1))
(eqv? (posn 1 1) (posn 1 1))
(eq? (posn 1 1) (posn 1 1))

(displayln "unequal list")
(equal? '(1 2 3) '(4 5 6))
(eqv? '(1 2 3) '(4 5 6))
(eq? '(1 2 3) '(4 5 6))

(displayln "equal list")
(equal? '(1 2 3) '(1 2 3))
(eqv? '(1 2 3) '(1 2 3))
(eq? '(1 2 3) '(1 2 3))
