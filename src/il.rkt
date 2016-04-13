#lang typed/racket/base

(provide (all-defined-out))

(define-type ILProgram ILStatement*)
(define-predicate ILProgram? ILProgram)
(define-type-alias ILModuleName Path)

(struct ILModule ([id   :  ILModuleName]
                  [provides : (Listof ILProvide)]
                  [requires : (Listof ILRequire)]
                  [body : ILStatement*])
  #:transparent)

(struct ILProvide ([id : Symbol]) #:transparent)
(struct ILRequire ([mod : ILModuleName] [idents* : (Listof Symbol)]) #:transparent)

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
                       ILField
                       ILSubscript
                       ILArray
                       ILObject
                       Symbol
                       ILValue))
(define-predicate ILExpr? ILExpr)

(struct ILLambda   ([args : (Listof Symbol)] [expr : ILStatement*]) #:transparent)
(struct ILBinaryOp ([operator : Symbol] [args : (Listof ILExpr)]) #:transparent)
(struct ILApp      ([lam : ILExpr] [args : (Listof ILExpr)]) #:transparent)
(struct ILArray    ([items : (Listof ILExpr)]) #:transparent)
(struct ILObject   ([items : (Listof (Pairof Symbol ILExpr))]) #:transparent)
(struct ILField    ([expr : ILExpr] [fieldname : Symbol]) #:transparent)
(struct ILSubscript ([expr : ILExpr] [fieldname : (U Symbol Natural)]) #:transparent)
(struct ILValue    ([v : Any]) #:transparent) ;; TODO: be more specific

;; TODO: This is quickfix. Maybe think more about this
(struct ILValuesMatch ([id : Symbol] [vref : Symbol] [index : Natural]) #:transparent)
(struct ILMultiValue ([v* : (Listof ILValue)]) #:transparent) ;; TODO: fits where?
