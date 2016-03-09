#lang typed/racket/base

(provide (all-defined-out))

(define-type ILProgram (Listof ILModule))
(define-predicate ILProgram? ILProgram)

(struct ILModule ([id   : (U String Symbol)]
                  ;; Provides
                  ;; Requires
                  [body : ILStatement*])
  #:transparent)

;;; IL Statements

(define-type ILStatement (U ILVarDec
                            ILReturn
                            ILIf
                            ILSeq
                            ILAssign
                            ILExpr))
(define-predicate ILStatement? ILStatement)

(define-type-alias ILStatement* (Listof ILStatement))

(struct ILVarDec   ([id : Symbol] [expr : ILExpr]) #:transparent)
(struct ILReturn   ([expr : ILExpr]) #:transparent)
(struct ILIf       ([pred : ILExpr]
                    [t-branch : ILStatement*]
                    [f-branch : ILStatement*])
  #:transparent)
(struct ILSeq      ([stms : ILStatement*]) #:transparent)
(struct ILAssign   ([id : Symbol] [rvalue : ILExpr]) #:transparent)

;;; IL Expressions

(define-type ILExpr (U ILFunction
                       ILBinaryOp
                       ILApp
                       ILValue))
(define-predicate ILExpr? ILExpr)

(struct ILFunction ([args : (Listof Symbol)] [expr : (Listof ILStatement)]) #:transparent)
(struct ILBinaryOp ([operator : Symbol] [right : ILExpr] [left : ILExpr]) #:transparent)
(struct ILApp      ([id : Symbol] [args : (Listof ILExpr)]) #:transparent)
(struct ILValue    ([v : Any]) #:transparent) ;; TODO: be more specific
