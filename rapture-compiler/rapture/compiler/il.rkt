#lang typed/racket/base

(provide (all-defined-out))

(require racket/match
         "language.rkt")


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
                         [body      : ILStatement*])
            (ILBinaryOp  [operator  : Symbol]
                         [args      : (Listof ILExpr)])
            (ILApp       [lam       : ILExpr]
                         [args      : (Listof ILExpr)])
            (ILArray     [items     : (Listof ILExpr)])
            (ILObject    [items     : (Listof (Pairof Symbol ILExpr))])
            (ILRef       [expr      : ILExpr]
                         [fieldname : Symbol])
            (ILIndex     [expr      : ILExpr]
                         [fieldexpr : ILExpr])
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
                 (ILReturn      [expr       : ILExpr])
                 (ILLabel       [name       : Symbol])
                 (ILContinue    [label      : Symbol])])


(: il-apply-optimization (-> ILModule (-> ILStatement* ILStatement*) ILModule))
(define (il-apply-optimization mod opt)
  (match-define (ILModule id p r b) mod)
  (ILModule id p r (opt b)))


(: ILObject-fields (-> ILObject (Listof Symbol)))
(define (ILObject-fields o)
  (for/list ([item (ILObject-items o)])
    (car item)))

(: ILObject-bodies (-> ILObject (Listof ILExpr)))
(define (ILObject-bodies o)
  (for/list ([item (ILObject-items o)])
    (cdr item)))
