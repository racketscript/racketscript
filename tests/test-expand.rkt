#lang racket

(require rackunit
         "../src/expand.rkt"
         "../src/absyn.rkt")


(define-syntax-rule (to-absyn/expand stx)
  (to-absyn/top (expand stx)))

;;; Check values

(check-equal? (to-absyn/expand #'42)
              (Quote 42))
(check-equal? (to-absyn/expand "Hello World")
              (Quote "Hello World"))
(check-equal? (to-absyn/expand #`'symbol)
              (Quote 'symbol))
(check-equal? (to-absyn/expand #'#(1 2 3 4))
              (Quote #(1 2 3 4))
              "vector")
(check-equal? (to-absyn/expand #''(1 2 3 4))
              (Quote '(1 2 3 4))
              "list")
(check-equal? (to-absyn/expand #''(1 2 "hi!" 3 #(1 2 3)))
              (Quote '(1 2 "hi!" 3 #(1 2 3))))
(check-equal? (to-absyn/expand #'#f)
              (Quote #f))


;; Check lambdas

(check-equal? (to-absyn/expand #`(λ (x) x))
              (PlainLambda '(x) (list 'x)))
(check-equal? (to-absyn/expand #`(λ x x))
              (PlainLambda 'x (list 'x)))
(check-equal? (to-absyn/expand #`(λ (a b . c) (+ a b (apply + c))))
              (PlainLambda
               '((a b) . c)
               (list
                (PlainApp '+
                          (list 'a 'b
                                (PlainApp 'apply (list '+ 'c)))))))

;; Check application

(check-equal? (to-absyn/expand #`(displayln "hello"))
              (PlainApp 'displayln (list (Quote "hello"))))
(check-equal? (to-absyn/expand #`((λ (x) x) 42))
              (PlainApp (PlainLambda '(x) '(x))
                        (list (Quote 42))))

;; If expresion

(check-equal? (to-absyn/expand #`(if #t 'yes 'no))
              (If (Quote #t) (Quote 'yes) (Quote 'no)))

;; Check TopId, should happen rarely
(check-equal? (to-absyn/expand #'x)
              (TopId 'x))

;; Check let values, lambdas, applications and more

(check-equal? (to-absyn/expand #'(let-values ([(a) 1] [(b) 2])
                                   a b))
              (LetValues (list (cons '(a) (Quote 1))
                               (cons '(b) (Quote 2)))
                         (list 'a 'b))
              "let values")
(check-equal? (to-absyn/expand #'(let-values ([(a) '(1 2)] [(b) (+ 2 4)])
                                   a b))
              (LetValues (list (cons '(a) (Quote '(1 2)))
                               (cons '(b) (PlainApp '+ (list (Quote 2) (Quote 4)))))
                         (list 'a 'b)))

(check-equal? (to-absyn/expand
               #`(define-values (fact)
                   (λ (n)
                     (if (zero? n)
                         0
                         (* n (fact (sub1 n)))))))
              (DefineValues '(fact)
                (PlainLambda
                 '(n)
                 (list
                  (If (PlainApp 'zero? '(n))
                      (Quote 0)
                      (PlainApp
                       '*
                       (list 'n
                             (PlainApp
                              (TopId 'fact)
                              (list (PlainApp 'sub1 (list 'n)))))))))))

(check-equal?
 (to-absyn/expand
  #`(letrec-values
        ([(even? odd?)
          (values
           (lambda (n)
             (or (zero? n)
                 (odd? (sub1 n))))
           (lambda (n)
             (or (not (zero? n))
                 (even? (sub1 n)))))])
      (or (even? 50))))
 (LetRecValues
  (list
   (cons
    '(even? odd?)
    (PlainApp
     'values
     (list
      (PlainLambda
       '(n)
       (list
        (LetValues
         (list (cons '(or-part) (PlainApp 'zero? '(n))))
         (list
          (If
           'or-part
           'or-part
           (PlainApp 'odd? (list (PlainApp 'sub1 '(n)))))))))
      (PlainLambda
       '(n)
       (list
        (LetValues
         (list (cons '(or-part) (PlainApp 'not (list (PlainApp 'zero? '(n))))))
         (list
          (If
           'or-part
           'or-part
           (PlainApp 'even? (list (PlainApp 'sub1 '(n)))))))))))))
  (list (PlainApp 'even? (list (Quote 50))))))
  
;;; Begin expressions

(check-equal?
 (to-absyn/expand #'(begin
                      (displayln "Hello!")
                      (displayln "Begin")
                      (displayln "Expression")))
 (list
  (PlainApp 'displayln (list (Quote "Hello!")))
  (PlainApp 'displayln (list (Quote "Begin")))
  (PlainApp 'displayln (list (Quote "Expression")))))

(check-equal?
 (to-absyn/expand #'(begin0 (displayln "Hello!")
                      (displayln "Begin")
                      (displayln "Expression")))
 (Begin0
   (PlainApp 'displayln (list (Quote "Hello!")))
   (list
    (PlainApp 'displayln (list (Quote "Begin")))
    (PlainApp 'displayln (list (Quote "Expression"))))))

(check-equal?
 (to-absyn/expand #'(define (foobar a b c)
                      (displayln a)
                      (displayln b)
                      (displayln c)))
 (DefineValues
   '(foobar)
   (PlainLambda
    '(a b c)
    (list
     (PlainApp 'displayln (list 'a))
     (PlainApp 'displayln (list 'b))
     (PlainApp 'displayln (list 'c))))))

;;; Case Lambda

(check-equal?
 (to-absyn/expand (expand #'(case-lambda
                              [(a b) (+ a b)]
                              [(a b c) (* a b c)])))
 (CaseLambda
  (list
   (PlainLambda '(a b) (list (PlainApp '+ '(a b))))
   (PlainLambda '(a b c) (list (PlainApp '* '(a b c)))))))

;;; Check module

(test-case "simple module"
  (define module-output (convert (expand
                                  #'(module foo racket/base
                                      (provide foo)
                                      (define (foo name)
                                        (displayln "Hello"))))
                                 (string->path "test-expand.rkt")))
  (check-equal? (Module-id module-output) 'foo)
  (check-equal? (Module-path module-output) (string->path "test-expand.rkt"))
  (check-equal? (Module-forms module-output)
                (list
                 (list (Provide 'foo))
                 (DefineValues
                   '(foo)
                   (PlainLambda '(name)
                                (list
                                 (PlainApp 'displayln (list (Quote "Hello")))))))))
