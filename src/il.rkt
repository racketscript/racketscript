#lang typed/racket/base

(provide (all-defined-out))

(define-type ILProgram ILStatement*)
(define-predicate ILProgram? ILProgram)
(define-type ModuleName (U String Symbol))

(struct ILModule ([id   :  ModuleName]
                  [provides : (Listof ModuleName)]
                  [requires : (Listof ModuleName)]
                  [body : ILStatement*])
  #:transparent)

;;; IL Statements

(define-type ILStatement (U ILVarDec
                            ILReturn
                            ILIf
                            ILValuesMatch
                            ILAssign
                            ILExpr
                            ILModule)) ;;NOTE: Adding ILModule here feels hacky
(define-predicate ILStatement? ILStatement)

(define-type-alias ILStatement* (Listof ILStatement))

(struct ILVarDec   ([id : Symbol] [expr : (Option ILExpr)]) #:transparent)
(struct ILReturn   ([expr : ILExpr]) #:transparent)
(struct ILIf       ([pred : ILExpr]
                    [t-branch : ILStatement*]
                    [f-branch : ILStatement*])
  #:transparent)
;; (struct ILSeq      ([stms : ILStatement*]) #:transparent)
(struct ILAssign   ([id : Symbol] [rvalue : ILExpr]) #:transparent)

;;; IL Expressions

(define-type ILExpr (U ILLambda
                       ILBinaryOp
                       ILApp
                       Symbol
                       ILValue))
(define-predicate ILExpr? ILExpr)

(struct ILLambda   ([args : (Listof Symbol)] [expr : ILStatement*]) #:transparent)
(struct ILBinaryOp ([operator : Symbol] [right : ILExpr] [left : ILExpr]) #:transparent)
(struct ILApp      ([lam : ILExpr] [args : (Listof ILExpr)]) #:transparent)
(struct ILValue    ([v : Any]) #:transparent) ;; TODO: be more specific

;; TODO: This is quickfix. Maybe think more about this
(struct ILValuesMatch ([id : Symbol] [vref : Symbol] [index : Natural]) #:transparent)
(struct ILMultiValue ([v* : (Listof ILValue)]) #:transparent) ;; TODO: fits where?
