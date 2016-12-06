#lang typed/racket/base

(require "language.rkt")

(provide (all-defined-out))

(struct Module  ([id      : Symbol]
                 [path    : Path]
                 [lang    : (U Symbol String (Listof Symbol))]
                 [imports : (Setof (U Path Symbol))]
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
                            (JSRequire [alias : Symbol]
                                       [path : (U Symbol Path-String)])
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
           (VarRef            [var      : (Option (U Symbol TopId))])
           (Quote             [datum    : Any])


           Begin
           (Begin0            [expr0    : Expr]
                              [expr*    : (Listof Expr)])

           (PlainApp          [lam      : Expr]    [args  : (Listof Expr)])
           (PlainLambda       [formals  : Formals] [exprs : (Listof Expr)])
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
          (ImportedIdent      [id : Symbol] [src-mod : Module-Path])
          (TopLevelIdent      [id : Symbol])]

  [Begin   (Listof TopLevelForm)]

  ;; Bindings and Formal Arguments

  [Binding      (Pairof Args Expr)]

  [Args         (Listof Symbol)]

  [Formals      Symbol
                (Listof Symbol)
                (Pairof (Listof Symbol) Symbol)])
