#lang typed/racket/base

(require "ident.rkt"
         "language.rkt")

(provide (all-defined-out))

(struct Linklet ([forms : (Listof GeneralTopLevelForm)]))

(struct Module  ([id      : Symbol]
                 [path    : Path]
                 [lang    : (U Symbol String (Listof Symbol))]
                 [imports : (Setof (U Path Symbol))]
                 [quoted-bindings : (Setof Symbol)]
                 [forms   : (Listof ModuleLevelForm)])
  #:transparent)

(define-language Absyn
  #:alias
  [Program      TopLevelForm]
  [ModuleName   (U Symbol Path)]

  #:forms
  ;; Top Level Forms

  [TopLevelForm             GeneralTopLevelForm
                            Expr
                            Module
                            Begin]

  [GeneralTopLevelForm      Expr
                            (DefineValues [ids : Args] [expr : Expr])
                            ;; DefineSyntaxes

                            ;; Same as ILRequire, but can't use to
                            ;; avoid cyclic depnedency.
                            (JSRequire [alias : Symbol]
                                       [path : (U Symbol Path-String)]
                                       [mode : (U 'default '*)])
                            #;Require*]

  ;; Module Level Forms
  [Provide*            (Listof Provide)]
  [Provide             (SimpleProvide     [id       : Symbol])
                       (RenamedProvide    [local-id : Symbol]
                                          [exported-id : Symbol])
                       (AllDefined        [exclude : (Setof Symbol)])
                       (PrefixAllDefined  [prefix-id : Symbol]
                                          [exclude : (Setof Symbol)])]
  [ModuleLevelForm     GeneralTopLevelForm
                       Provide*
                       SubModuleForm]

  [SubModuleForm       Module]

  ;; Expressions

  [Expr    Ident
           (TopId             [id       : Symbol])
           (VarRef            [var      : (Option (U Symbol TopId Ident))])
           (Quote             [datum    : Any])


           Begin
           (Begin0            [expr0    : Expr]
                              [expr*    : (Listof Expr)])

           (PlainApp          [lam      : Expr]    [args  : (Listof Expr)])
           (PlainLambda       [formals  : Formals] [exprs : (Listof Expr)] [unchecked? : Boolean])
           (CaseLambda        [clauses  : (Listof PlainLambda)])


           (If                [pred     : Expr]
                              [t-branch : Expr]
                              [f-branch : Expr])

           ;; This also acts as LetRecValues because Absyn is
           ;; freshened.
           (LetValues         [bindings : (Listof Binding)]
                              [body     : (Listof Expr)])
           (Set!              [id       : Symbol] [expr : Expr])

           (WithContinuationMark   [key    : Expr]
                                   [value  : Expr]
                                   [result : Expr])]

  [Ident  (LocalIdent         [id : Symbol])
          (ImportedIdent      [id : Symbol] [src-mod : Module-Path] [reachable? : Boolean])
          (TopLevelIdent      [id : Symbol])]

  [Begin   (Listof TopLevelForm)]

  ;; Bindings and Formal Arguments

  [Binding      (Pairof Args Expr)]

  [Args         (Listof Symbol)]

  [Formals      Symbol
                (Listof Symbol)
                (Pairof (Listof Symbol) Symbol)])


(: lambda-arity (-> PlainLambda (U Natural arity-at-least)))
(define (lambda-arity f)
  (define frmls (PlainLambda-formals f))
  (cond
    [(symbol? frmls) (arity-at-least 0)]
    [(list? frmls) (length frmls)]
    [(pair? frmls) (arity-at-least (length (car frmls)))]))

(: variadic-lambda? (-> PlainLambda Boolean))
(define (variadic-lambda? lam)
  (not (list? (PlainLambda-formals lam))))

(: plain-lambda-arity-includes (-> PlainLambda Natural Boolean))
(define (plain-lambda-arity-includes lam k)
  (formals-arity-includes (PlainLambda-formals lam) k))

(: formals-arity-includes (-> Formals Natural Boolean))
(define (formals-arity-includes formals k)
  (cond
    [(symbol? formals) #t]
    [(list? formals) (equal? (length formals) k)]
    [(pair? formals) (>= k (length (car formals)))]))

(: freshen-formals (-> Formals Formals))
(define (freshen-formals formals)
  (cond
    [(symbol? formals) (fresh-id (string->symbol (format "_~a" formals)))]
    [(list? formals) (for/list ([f : Symbol formals]) : (Listof Symbol)
                       (cast (freshen-formals f) Symbol))]
    [(pair? formals) (cons (cast (freshen-formals (car formals)) (Listof Symbol))
                           (cast (freshen-formals (cdr formals)) Symbol))]))

(: formals->list (-> Formals (Listof Symbol)))
(define (formals->list formals)
  (cond
    [(symbol? formals) (list formals)]
    [(list? formals) formals]
    [(pair? formals) (append (car formals)
                             (list (cdr formals)))]))
