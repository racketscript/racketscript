#lang racket/base

(require (only-in racket/list flatten)
         "ast.rkt"
         "struct-match.rkt")

(provide (all-defined-out)
         (prefix-out IL
                     (combine-out
                       (struct-out SimpleProvide)
                       (struct-out RenamedProvide)
                       (struct-out IfClause))))

;; (ILLinklet [Listof ILRequire?] [Listof ILProvide?] [Listof ILStatement?])
(struct ILLinklet (imports exports body) #:transparent)

;; (ILRequire [Maybe path-string?] symbol? [or 'default '*])
(struct ILRequire (mod name import-mode) #:transparent)

;; ILProvide is one of:
;; - SimpleProvide
;; - RenamedProvide

;; TODO purpose unclear?
(struct IfClause (pred body) #:transparent)

;; (ILCheckedFormals Formals?)
(struct ILCheckedFormals (formals) #:transparent)

;; ILExpr is one of the following, along with symbol?:

;; (ILLambda [or Formals? ILCheckedFormals?] [Listof ILStatement?])
(struct ILLambda (args body) #:transparent)

;; (ILBinaryOp symbol? [Listof ILExpr?])
(struct ILBinaryOp (operator args) #:transparent)

;; (ILApp ILExpr? [Listof ILExpr?])
(struct ILApp (lam args) #:transparent)

;; (ILArray [Listof ILExpr?])
(struct ILArray (items) #:transparent)

;; (ILObject [Listof (cons (or string? symbol?) ILExpr?)])
(struct ILObject (items) #:transparent)

;; (ILRef ILExpr? symbol?)
(struct ILRef (expr fieldname) #:transparent)

;; (ILIndex? ILExpr? ILExpr?)
(struct ILIndex (expr fieldexpr) #:transparent)

;; (ILValue any?)
(struct ILValue (v) #:transparent)

;; (ILNew [or (or sumbol? ILRef? ILIndex?) ILApp?])
(struct ILNew (v) #:transparent)

;; (ILInstanceOf ILExpr? ILExpr?)
(struct ILInstanceOf (expr type) #:transparent)

;; (ILTypeOf ILExpr?)
(struct ILTypeOf (expr) #:transparent)

(struct ILNull () #:transparent)

(struct ILUndefined () #:transparent)

(struct ILArguments () #:transparent)

(struct ILThis () #:transparent)

;; an ILStatement is one of (along with ILExpr):

;; (ILVarDec symbol? [Maybe ILExpr?])
(struct ILVarDec (id expr) #:transparent)

;; (ILLetDec symbol? [Maybe ILExpr?])
(struct ILLetDec (id expr) #:transparent)

;; (ILIf ILExpr? [Listof ILStatement?] [Listof ILStatement?])
(struct ILIf (pred t-branch f-branch) #:transparent)

;; (ILIf* [Listof IfClause?])
(struct ILIf* (clauses) #:transparent)

;; (ILAssign (or sumbol? ILRef? ILIndex?) ILExpr?)
(struct ILAssign (lvalue rvalue) #:transparent)

;; (ILWhile ILExpr? [Listof ILStatement?])
(struct ILWhile (condition body) #:transparent)

;; (ILReturn ILExpr?)
(struct ILReturn (expr) #:transparent)

;; (ILLabel symbol?)
(struct ILLabel (name) #:transparent)

;; (ILContinue symbol?)
(struct ILContinue (label) #:transparent)

;; (ILExnHandler [Listof ILStatement?]
;;               symbol?
;;               [Listof ILStatement?]
;;               [Listof ILStatement?]
;;               ILExpr?)
(struct ILExnHandler (try error catch finally) #:transparent)

;; (ILThrow ILExpr?)
(struct ILThrow (expr) #:transparent)


(define (ILExpr? e)
  (or (ILLambda? e)
      (ILBinaryOp? e)
      (ILApp? e)
      (ILArray? e)
      (ILObject? e)
      (ILRef? e)
      (ILIndex? e)
      (ILValue? e)
      (ILNew? e)
      (ILInstanceOf? e)
      (ILTypeOf? e)
      (ILNull? e)
      (ILUndefined? e)
      (ILArguments? e)
      (ILThis? e)
      (symbol? e)))
        
(define (ILStatement? e)
  (or (ILExpr? e)
      (ILVarDec? e)
      (ILIf? e)
      (ILIf*? e)
      (ILAssign? e)
      (ILWhile? e)
      (ILReturn? e)
      (ILLabel? e)
      (ILContinue? e)
      (ILExnHandler? e)
      (ILThrow? e)))

(define (ILStatement*? e) (and (list? e) (andmap ILStatement? e)))

(define (ILObject-fields o)
  (for/list ([item (ILObject-items o)])
    (car item)))

(define (ILObject-bodies o)
  (for/list ([item (ILObject-items o)])
    (cdr item)))

(define (il-formals-arity-includes formals k)
  (struct-match formals
    [(ILCheckedFormals formals*)
     (formals-arity-includes formals* k)]
    [v (formals-arity-includes v k)]))

(define (il-freshen-formals formals)
  (struct-match formals
    [(ILCheckedFormals formals*)
     (ILCheckedFormals (freshen-formals formals*))]
    [v (freshen-formals v)]))

(define (il-formals->list formals)
  (struct-match formals
    [(ILCheckedFormals formals*)
     (formals->list formals*)]
    [v (formals->list v)]))

(define (flatten-statements stms) (flatten stms))
