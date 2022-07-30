#lang racket/base

(provide (all-defined-out))

;; (ILLinklet [Listof ILRequire?] [Listof ILProvide?] [Listof ILStatement?])
(struct ILLinklet (imports exports body) #:transparent)

;; (ILRequire [Maybe path-string?] symbol? [or 'default '*])
(struct ILRequire (mod name import-mode) #:transparent)

;; TODO purpose unclear?
(struct IfClause (pred body) #:transparent)

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

;; (ILInstanceof ILExpr? ILExpr?)
(struct ILInstanceof (expr type) #:transparent)

;; (ILTypeof ILExpr?)
(struct ILTypeof (expr) #:transparent)

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
