#lang racket/base

(require "ident.rkt")

(provide (all-defined-out))

;; TODO path should get eliminated once things are bootstrapped (or perhaps just before)
(struct Linklet (path imports exports forms) #:transparent)

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

;; (SimpleProvide symbol?)
(struct SimpleProvide (id) #:transparent)

;; (RenamedProvide symbol? symbol?)
(struct RenamedProvide (local-id exported-id) #:transparent)

(define (formals-arity-includes formals k)
  (cond
    [(symbol? formals) #t]
    [(list? formals) (equal? (length formals) k)]
    [(pair? formals) (>= k (length (car formals)))]))

(define (freshen-formals formals)
  (cond
    [(symbol? formals) (fresh-id (string->symbol (format "_~a" formals)))]
    [(list? formals) (for/list ([f formals])
                       (freshen-formals f))]
    [(pair? formals) (cons (freshen-formals (car formals))
                           (freshen-formals (cdr formals)))]))

(define (formals->list formals)
  (cond
    [(symbol? formals) (list formals)]
    [(list? formals) formals]
    [(pair? formals) (append (car formals)
                             (list (cdr formals)))]))

(define (Ident? e)
  (or (ImportedIdent? e)
      (TopLevelIdent? e)
      (LinkletImportIdent? e)
      (LocalIdent? e)))

;; NOTE the linklet docs specify that:
;;      1. quote-syntax and #%top don't appear in the IR
;;      2. #%plain-app is implicit
;;      3. #%plain-lambda is 'spelled' lambda
(define (Expr? e)
  (or (Ident? e)
      (VarRef? e)
      (Quote? e)
      (Begin? e)
      (Begin0? e)
      (App? e)
      (Lambda? e)
      (CaseLambda? e)
      (If? e)
      (LetValues? e)
      (Set!? e)
      (WithContinuationMark? e)))

(define (lambda-arity f)
  (define frmls (Lambda-formals f))
  (cond
    [(symbol? frmls) (arity-at-least 0)]
    [(list? frmls) (length frmls)]
    [(pair? frmls) (arity-at-least (length (car frmls)))]))

(define (variadic-lambda? lam)
  (not (list? (Lambda-formals lam))))
