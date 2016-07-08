#lang racket

(require rackunit
         "../src/transform.rkt"
         "../src/absyn.rkt"
         "../src/il.rkt"
         "../src/config.rkt"
         "../src/environment.rkt"
         "../src/expand.rkt"
         "../src/util.rkt")


(define-syntax-rule (values->list e)
  (call-with-values (λ () e) list))

(define-syntax-rule (check-ilexpr expr stms val)
  (parameterize ([fresh-id-counter 0])
    (check-equal?
     (values->list (absyn-expr->il expr))
     (list stms val))))

;; enable test environment
(test-environment? #t)

;;; Expressions ---------------------------------------------------------------

;; Values

(check-ilexpr (Quote 42)        '() (ILValue 42))
(check-ilexpr (Quote "Hello")   '() (ILValue "Hello"))
(check-ilexpr (Quote 'hello)    '() (ILValue 'hello))
(check-ilexpr (Quote '(1 2 3))  '() (ILValue '(1 2 3)))
(check-ilexpr (Quote '(1 (2 3) 4 (a b))) '() (ILValue '(1 (2 3) 4 (a b))))
(check-ilexpr (Quote '(1 . 2))  '() (ILValue '(1 . 2)))
(check-ilexpr (Quote #f)        '() (ILValue #f))

;; Function application

(check-ilexpr (PlainLambda '(x) (list 'x))
              '()
              (ILLambda '(x) (list (ILReturn 'x))))
(check-ilexpr (PlainLambda 'x (list 'x))
              '()
              (ILLambda
               '()
               (list
                (ILVarDec
                 'x
                 (ILApp
                  (name-in-module 'core 'Pair.listFromArray)
                  (list (ILApp (name-in-module 'core 'argumentsToArray) '(arguments)))))
                (ILReturn 'x))))

;; If expressions

(check-ilexpr (If (Quote #t) (Quote 'yes) (Quote 'no))
              (list
               (ILIf
                (ILValue #t)
                (list (ILVarDec 'if_res1 (ILValue 'yes)))
                (list (ILVarDec 'if_res1 (ILValue 'no)))))
              'if_res1)

;; Lambdas

(check-ilexpr (PlainLambda '(x) (list 'x))
              '()
              (ILLambda
               '(x)
               (list (ILReturn 'x))))
(check-ilexpr (PlainLambda '(a b)
                           (list (PlainApp 'list '(a b))))
              '()
              (ILLambda '(a b)
                        (list (ILReturn (ILApp 'list '(a b))))))
                        

;; Let expressions
(check-ilexpr (LetValues (list (cons '(a) (Quote 1))
                               (cons '(b) (Quote 2)))
                         '(a b))
              (list (ILVarDec 'a (ILValue 1)) (ILVarDec 'b (ILValue 2))
                    'a)
              'b)
(check-ilexpr (LetValues (list (cons '(a) (If (Quote #t) (Quote 'yes) (Quote 'false)))
                               (cons '(b) (PlainApp '+ (list (Quote 1) (Quote 2)))))
                         (list (PlainApp 'list '(a b))))
              (list
               (ILIf
                (ILValue #t)
                (list (ILVarDec 'if_res1 (ILValue 'yes)))
                (list (ILVarDec 'if_res1 (ILValue 'false))))
               (ILVarDec 'a 'if_res1)
               (ILVarDec 'b (ILBinaryOp '+ (list (ILValue 1) (ILValue 2)))))
              (ILApp 'list '(a b)))


;; Binary operations

(check-ilexpr (PlainApp '+ (list (Quote 1) (Quote 2)))
              '()
              (ILBinaryOp '+ (list (ILValue 1) (ILValue 2))))
(check-ilexpr (PlainApp '- (list (Quote 1) (Quote 2) (Quote 3)))
              '()
              (ILBinaryOp '- (list (ILValue 1) (ILValue 2) (ILValue 3))))

;; Case Lambda

(check-ilexpr (CaseLambda
               (list
                (PlainLambda '(a b) (list (PlainApp '+ '(a b))))
                (PlainLambda '(a b c) (list (PlainApp '* '(a b c))))))
              '()
              (ILLambda
               '()
               (list
                (ILVarDec
                 'args1
                 (ILApp
                  (name-in-module 'core 'Pair.listFromArray)
                  (list (ILApp (name-in-module 'core 'argumentsToArray) '(arguments)))))
                (ILIf
                 (ILApp
                  (name-in-module 'kernel 'equal_p)
                  (list (ILApp (name-in-module 'kernel 'length) '(args1)) (ILValue 2)))
                 (list
                  (ILVarDec
                   'if_res3
                   (ILApp
                    (name-in-module 'kernel 'apply)
                    (list
                     (ILLambda '(a b) (list (ILReturn (ILBinaryOp '+ '(a b)))))
                     'args1))))
                 (list
                  (ILIf
                   (ILApp
                    (name-in-module 'kernel 'equal_p)
                    (list (ILApp '$rjs_kernel.length '(args1)) (ILValue 3)))
                   (list
                    (ILVarDec
                     'if_res2
                     (ILApp
                      (name-in-module 'kernel 'apply)
                      (list
                       (ILLambda '(a b c) (list (ILReturn (ILBinaryOp '* '(a b c)))))
                       'args1))))
                   (list
                    (ILVarDec
                     'if_res2
                     (ILApp 'error (list (ILValue "case-lambda: invalid case"))))))
                  (ILVarDec 'if_res3 'if_res2)))
                (ILReturn 'if_res3))))

;; FFI
