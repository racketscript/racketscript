#lang racket

(require (for-syntax syntax/parse
                     racket/stxparam))

(provide s-case-lambda)

;; s-case-lambda : Syntax -> Syntax
;; Transforms case-lambda to plain-lambda using nested if-else
(define-syntax (s-case-lambda stx)
  (define-syntax-class formals
    #:description "formal arguments"
    (pattern (~or var:id
                  (v0:id ...)
                  (v1:id ...+ . v1*:id))))
  (define-syntax-class clause
    #:description "A single case-lambda clause"
    (pattern (f:formals body:expr ...+)))
  
  ;; formals-check : Syntax Syntax:Formals -> stx
  ;; Produce expression to check if arguments given at `args`
  ;; from the driver lambda is valid for current case
  (define (formals-check stx)
    (syntax-parse stx
      [(args:expr var:id) #'(list? args)]
      [(args:expr (var:id ...))
       #:with len (datum->syntax stx (length (syntax-e #'(var ...))))
       #'(equal? (length args) len)]
      [(args:expr (var:id ...+ . vtl:id))
       #:with len (datum->syntax stx (length (syntax-e #'(var ...))))
       #'(>= (length args) len)]))

  ;; transform : Syntax -> Syntax
  ;; Transforms each clause of case-lambda to nested if-else
  ;; expression
  (define (transform stx)
    (syntax-parse stx
      [(args:expr) #'(error "No match for case-lambda")]
      [(args:expr (fs:formals body:expr ...+) c:clause ...)
       #:with check-expr (formals-check #'(args fs))
       #:with rest-clauses (transform #'(args c ...))
       #'(if check-expr
             (apply (λ fs body ...) args)
             rest-clauses)]))
  
  (syntax-parse stx
    #:literals (λ)
    [(c-λ (fs:formals body:expr ...+) ...)
     #:with body* (transform #'(args (fs body ...) ...))
     #'(λ args body*)]))

(module+ test
  (require rackunit)
  
  (define lam1
    (s-case-lambda
     [(a b c) (* a b c)]
     [(a b) (+ a b)]))

  (check-equal? (lam1 8 2 3) (* 8 2 3) "match with first case")
  (check-equal? (lam1 3 4) (+ 3 4) "match with second case")
  (check-exn exn:fail? (λ () (lam1 3 4 5 6)) "no matching clause")

  (define lam2
    (s-case-lambda
     [() "duh"]
     [(a b c) (* a b c)]
     [(a b) (+ a b)]
     [v (apply / v)]))

  (check-equal? (lam2) "duh" "case without any arguments")
  (check-equal? (lam2 8 2 3) (* 8 2 3) "match with first case")
  (check-equal? (lam2 3 4) (+ 3 4) "match with second case")
  (check-equal? (lam2 3 4 5 6) (/ 3 4 5 6) "match the variable arg clause")

  (define lam3
    (s-case-lambda
     [() "duh"]
     [(a b c) (* a b c)]
     [(a b) (+ a b)]
     [(a b c . d) (+ a b c (apply * d))]
     [v (apply / v)]))

  (check-equal? (lam3) "duh" "case without any arguments")
  (check-equal? (lam3 8 2 3) (* 8 2 3) "match with first case")
  (check-equal? (lam3 3 4) (+ 3 4) "match with second case")
  (check-equal? (lam3 3 4 5 6) (+ 3 4 5 6) "match the variable arg clause")
  (check-equal? (lam3 3 4 5 6 7 8 9) (+ 3 4 5 (* 6 7 8 9))
                "match the variable arg clause"))


