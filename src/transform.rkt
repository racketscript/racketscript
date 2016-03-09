#lang typed/racket/base

;;; Generate IL code from abstract syntax. Each binding name
;;; in assumed to be fresh, to enforce lexical scope rules of Racket

(require racket/match
         racket/function
         "config.rkt"
         "util.rkt"
         "absyn.rkt"
         "il.rkt")

(provide absyn->il)

(: absyn->il (-> Program Void))
(define (absyn->il p)
  (cond
    [(GeneralTopLevelForm? p) (void)]
    [(Expr? p) (void)]
    [(Module? p) (void)]
    [(Begin? p) (void)]))

(: absyn-expr->il (-> Expr (Values ILStatement* ILExpr)))
;;; An expression in Racket may need to be split into several
;;; statements in JS. However, since expression always has a
;;; values, we return pair of statements and the final value
;;; of expression.
(define (absyn-expr->il expr)
  (match expr
    [(PlainLambda args exprs)
     ;;; TODO: URGENT! add 'return' statement at end
     (values '() 'void)]
    [(If pred-e t-branch f-branch)
     (define-values (ps pe) (absyn-expr->il pred-e))
     (define-values (ts te) (absyn-expr->il t-branch))
     (define-values (fs fe) (absyn-expr->il f-branch))
     (define result-id (fresh-id 'if_res))
     (values (append (list (ILVarDec result-id #f))
                     ps
                     (list (ILIf pe
                                 (append1 ts (ILVarDec result-id te))
                                 (append1 fs (ILVarDec result-id fe)))))
             result-id)]
    [(LetValues bindings body)
     (define binding-stms
       (for/fold ([stms '()])
                 ([b bindings])
         (define-values (s v) (absyn-expr->il b))
         (append stms
                 (list s (ILVarDec b v)))))
     (define-values (s v) (absyn-exp->il body))
       ;;; TODO: let needs pattern matching here:
                            (void))
     (values '() 'void)
     ]
    [(Set! id e)
     (values '() 'void)
     ]
    [(PlainApp lam args)
     (values '() 'void)  
     ]
    [(TopId id) (values '() id)] ;; FIXME: rename top-levels?
    [(Quote datum) (values '() (absyn-value->il expr))]
    [(cons hd tl) (values '() 'void) ]
    [_ #:when (symbol? expr) (values '() expr)]
    [_ (error "unsupported expr")]))

(: absyn-value->il (-> Any ILValue))
(define (absyn-value->il d)
  (cond
    [(or (symbol? d)
         (string? d)
         (integer? d)
         (list? d)
         (real? d))
     (ILValue d)]
    [else (error "unsupported value")]))

