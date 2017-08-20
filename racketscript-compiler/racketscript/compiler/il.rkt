#lang typed/racket/base

(require racket/match
         racket/list
         "absyn.rkt"
         "language.rkt")

(provide (all-defined-out)
         (prefix-out IL
                     (combine-out
                      (struct-out SimpleProvide)
                      (struct-out RenamedProvide)
                      (struct-out IfClause))))

(define-type        ILProgram ILStatement*)
(define-predicate   ILProgram? ILProgram)
(define-type-alias  ILModuleName (Option Path-String))

(struct ILModule ([id       : ILModuleName]
                  [provides : ILProvide*]
                  [requires : ILRequire*]
                  [body     : ILStatement*])
  #:transparent)

(struct ILRequire ([mod         : ILModuleName]
                   [name        : Symbol]
                   [import-mode : (U 'default '*)]) #:transparent)
(define-type ILRequire* (Listof ILRequire))

(struct IfClause ([pred : (Option ILExpr)]
                  [body : ILStatement*]) #:transparent)


(define-language IL
  #:alias
  [ILStatement*    (Listof ILStatement)]
  [ObjectKey       (U String Symbol)]
  [ILLValue        (U Symbol ILRef ILIndex)]
  [ObjectPair      (Pairof ObjectKey ILExpr)]

  #:forms
  [ILExpr   (ILLambda      [args      : (Listof Symbol)]
                           [body      : ILStatement*])
            (ILBinaryOp    [operator  : Symbol]
                           [args      : (Listof ILExpr)])
            (ILApp         [lam       : ILExpr]
                           [args      : (Listof ILExpr)])
            (ILArray       [items     : (Listof ILExpr)])
            (ILObject      [items     : (Listof ObjectPair)])
            (ILRef         [expr      : ILExpr]
                           [fieldname : Symbol])
            (ILIndex       [expr      : ILExpr]
                           [fieldexpr : ILExpr])
            (ILValue       [v         : Any])
            (ILNew         [v         : (U ILLValue ILApp)])
            (ILInstanceOf  [expr      : ILExpr]
                           [type      : ILExpr])
            (ILTypeOf      [expr      : ILExpr])

            ;; Should be ideally in values
            (ILNull)
            (ILUndefined)
            (ILArguments)
            (ILThis)
            Symbol]

  [ILStatement   ILExpr
                 (ILVarDec      [id         : Symbol]
                                [expr       : (Option ILExpr)])
                 (ILLetDec      [id         : Symbol]
                                [expr       : (Option ILExpr)])
                 (ILIf          [pred       : ILExpr]
                                [t-branch   : ILStatement*]
                                [f-branch   : ILStatement*])
                 (ILIf*         [clauses    : (Listof IfClause)])
                 (ILAssign      [lvalue     : ILLValue]
                                [rvalue     : ILExpr])
                 (ILWhile       [condition  : ILExpr]
                                [body       : ILStatement*])
                 (ILReturn      [expr       : ILExpr])
                 (ILLabel       [name       : Symbol])
                 (ILContinue    [label      : Symbol])
                 (ILExnHandler  [try        : ILStatement*]
                                [error      : Symbol]
                                [catch      : ILStatement*]
                                [finally    : ILStatement*])
                 (ILThrow       [expr       : ILExpr])]

  [ILProvide*    (Listof ILProvide)]
  [ILProvide     SimpleProvide
                 RenamedProvide])

(: il-apply-optimization (-> ILModule (-> ILStatement* ILStatement*) ILModule))
(define (il-apply-optimization mod opt)
  (match-define (ILModule id p r b) mod)
  (ILModule id p r (opt b)))


(: ILObject-fields (-> ILObject (Listof ObjectKey)))
(define (ILObject-fields o)
  (for/list ([item (ILObject-items o)])
    (car item)))

(: ILObject-bodies (-> ILObject (Listof ILExpr)))
(define (ILObject-bodies o)
  (for/list ([item (ILObject-items o)])
    (cdr item)))

(: flatten-statements (-> (Listof Any) ILStatement*))
(define (flatten-statements stms)
  (cast (flatten (cast stms (Listof Any))) ILStatement*))
