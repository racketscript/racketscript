#lang racket/base

(provide (all-defined-out))

(struct Linklet (imports exports forms) #:transparent)

(struct DefineValues (ids expr))

;; An Ident is one of:
;; - TopLevelIdent
;; - LinkletImportIdent
;; - LocalIdent
;; - ImportedIdent

;; TODO
;; should get rid of this, everything should be explicitly imported
;; via a linklet import or automatically associated with a primitive,
;; rather than the current hacky implementation of saying every variable
;; we can't identify is an import from #%kernel
;;
;; maybe use PrimitiveIdent?
(struct ImportedIdent (id src-mod reachable?) #:transparent)

(struct TopLevelIdent (id) #:transparent)
(struct LinkletImportIdent (id) #:transparent)
(struct LocalIdent (id) #:transparent)

;; TODO
;; Other variable forms that Pycket uses include:
;; - ModuleVar
;; - CellRef

;; VariableRef or #%variable

;; Expr

;; a Binding is a (cons [Listof symbol?] Expr?)
;; a Formals is one of:
;; - symbol?
;; - [Listof symbol?]
;; - (cons [Listof symbol?] symbol?)

;; (Quote datum?)
(struct Quote (datum) #:transparent)

;; (Begin [Listof Expr?])
(struct Begin (exprs) #:transparent)

;; (Begin0 Expr? [Listof Expr?])
(struct Begin0 (expr0 exprs) #:transparent)

;; (App Expr? [Listof Expr?])
(struct App (lam args) #:transparent)

;; (Lambda [Formals? [Listof Expr?] bool?)
(struct Lambda (formals exprs unchecked?) #:transparent)

;; (CaseLambda [Listof Lambda?])
(struct CaseLambda (clauses) #:transparent)

;; (If bool? Expr? Expr?)
(struct If (pred t-branch f-branch) #:transparent)

;; (LetValues [Listof Binding?] [Listof Expr?])
(struct LetValues (bindings body) #:transparent)

;; (Set! Ident? Expr?)
(struct Set! (id expr) #:transparent)

;; (WithContinuationMark Expr? Expr? Expr?)
(struct WithContinuationMark (key value result) #:transparent)

;; (VarRef (or false? Ident?))
(struct VarRef (id) #:transparent)

;; (JSRequire symbol? (or symbol? path-string?) (or 'default '*))
(struct JSRequire (alias path mode) #:transparent)
