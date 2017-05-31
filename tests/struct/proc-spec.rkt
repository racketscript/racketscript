#lang racket

(displayln "Test proc-spec when given as part of struct-type")
(displayln "Creating struct type.")
(define-values (struct:posn make-posn posn? posn-ref posn-set!)
  (make-struct-type 'posn #f 2 0 #f '() #f
                    (λ (p0 p1)
                      (+ (sqr (- (posn-ref p1 0) (posn-ref p0 0)))
                         (sqr (- (posn-ref p1 1) (posn-ref p0 1)))))))

(displayln "Creating posn instances.")
(define a (make-posn 0 0))
(define b (make-posn 3 4))

(displayln "Applying struct object.")
(displayln (a b))

(displayln "Checking struct predicated.")
(displayln (posn? a))
(displayln (posn? b))
(displayln (posn? (λ (x) x)))

(displayln "Checking struct accessors.")
(displayln (posn-ref a 0))
(displayln (posn-ref a 1))
(displayln (posn-ref b 0))
(displayln (posn-ref b 1))

(displayln "Test proc-spec when given as part of struct instance")
(displayln "Creating struct type.")
(define-values (struct:foo make-foo foo? foo-ref foo-set!)
  (make-struct-type 'foo #f 2 0 #f '() #f 0))

(displayln "Creating posn instances.")
(define x (make-foo (λ (x) (sqr x)) 42))

(displayln "Applying struct object.")
(displayln (x 10))

(displayln "Checking struct predicated.")
(displayln (foo? x))
(displayln (foo? (λ (x) x)))

(displayln "Checking struct accessors.")
(displayln (procedure? (foo-ref x 0)))
(displayln (foo-ref x 1))
