#lang typed/racket/base

;;; Generate JavaScript code from abstract syntax. Each binding name
;;; in assumed to be fresh, to enforce lexical scope rules of Racket
;;; in JavaScript

(require racket/string
         racket/format
         racket/match
         racket/function
         "config.rkt"
         "absyn.rkt")

(provide assemble)

(: normalize-symbol (-> Symbol String))
(define (normalize-symbol s)
  ;; TODO: handle every weird character in symbol
  ;; Since every identifier is suffixed with fresh symbol
  ;; we don't have to worry about name clashes after this
  ;; naive renaming
  (string-replace (~a s) "-" "_"))

(: assemble (-> Program Void))
(define (assemble p)
  (cond
    [(GeneralTopLevelForm? p) (void)]
    [(Expr? p) (void)]
    [(Module? p) (void)]
    [(Begin? p) (void)]))

(: assemble-expr (-> Expr Output-Port Void))
(define (assemble-expr expr out)
  (define emit (curry fprintf out))
  (match expr
    [(PlainLambda args exprs)
     ;;; TODO: URGENT! add 'return' statement at end
     (emit "function(")
     (emit (string-join (map normalize-symbol args) ", "))
     (emit ") {")
     (for ([e exprs])
       (assemble-expr e out))
     (emit "}")]
    [(If expr t-branch f-branch)
     (emit "if (")
     (assemble-expr expr out)
     (emit ") {")
     (assemble-expr t-branch out)
     (emit "} else { ")
     (assemble-expr f-branch out)
     (emit "}")]
    [(LetValues bindings body)
     (for ([b bindings])
       ;;; TODO: let needs pattern matching here
       (void))]
    [(Set! id e)
     (emit (~a (normalize-symbol id)))
     (emit " = ")
     (assemble-expr e out)]
    [(PlainApp lam args)
     (void)
       
     ]
    [(TopId id) (void)] ;; FIXME: rename top-levels?
    [(Quote datum) (assemble-value datum out)]
    [(cons hd tl) (void) ]
    [_ #:when (symbol? expr) (void)]
    [_ (error "unsupported expr" (void))]))

(: assemble-value (-> Any Output-Port Void))
(define (assemble-value d)
  ;; TODO
  (~a d))
