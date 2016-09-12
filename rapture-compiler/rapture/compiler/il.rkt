#lang typed/racket/base

(provide (all-defined-out))

(require "language.rkt")


(define-type        ILProgram ILStatement*)
(define-predicate   ILProgram? ILProgram)
(define-type-alias  ILModuleName (Option Path-String))

(struct ILModule ([id   :  ILModuleName]
                  [provides : (Listof ILProvide)]
                  [requires : (Listof ILRequire)]
                  [body : ILStatement*])
  #:transparent)

(struct ILProvide ([id : Symbol]) #:transparent)
(struct ILRequire ([mod : ILModuleName] [name : Symbol]) #:transparent)


(define-language IL
  #:alias
  [ILStatement*    (Listof ILStatement)]
  [ILLValue        (U Symbol ILRef ILIndex)]

  #:forms
  [ILExpr   (ILLambda    [args      : (Listof Symbol)]
                         [expr      : ILStatement*])
            (ILBinaryOp  [operator  : Symbol]
                         [args      : (Listof ILExpr)])
            (ILApp       [lam       : ILExpr]
                         [args      : (Listof ILExpr)])
            (ILArray     [items     : (Listof ILExpr)])
            (ILObject    [items     : (Listof (Pairof Symbol ILExpr))])
            (ILRef       [expr      : ILExpr]
                         [fieldname : Symbol])
            (ILIndex     [expr      : ILExpr]
                         [fieldname : ILExpr])
            (ILValue     [v         : Any])
            (ILNew       [v         : (U Symbol ILRef ILIndex ILApp)])
            Symbol]

  [ILStatement   ILExpr
                 (ILVarDec      [id         : Symbol]
                                [expr       : (Option ILExpr)])
                 (ILIf          [pred       : ILExpr]
                                [t-branch   : ILStatement*]
                                [f-branch   : ILStatement*])
                 (ILAssign      [lvalue     : ILLValue]
                                [rvalue     : ILExpr])
                 (ILWhile       [condition  : ILExpr]
                                [body       : ILStatement*])
                 (ILReturn      [expr       : ILExpr])])
