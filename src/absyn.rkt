#lang typed/racket/base

(provide (all-defined-out))

(define-type-alias Program TopLevelForm)

(define-type Expr (U Symbol
                     PlainLambda
                     CaseLambda
                     PlainApp
                     If
                     Begin
                     LetValues
                     LetRecValues
                     TopId
                     Set!
                     Quote
                     ;; QuoteSyntax
                     ;; WithContinuationMark
                     VarRef))
(define-predicate Expr? Expr)

(define-type TopLevelForm (U GeneralTopLevelForm
                             Expr
                             Module
                             Begin
                             ;; #%expression
                             ;; BeginForSyntax
                             ))
(define-predicate TopLevelForm? TopLevelForm)

(define-type ModuleLevelForm (U GeneralTopLevelForm
                                Provide
                                ;; BeginForSyntax
                                SubModuleForm
                                ;; Declare
                                ))
(define-predicate ModuleLevelForm? ModuleLevelForm)

(define-type SubModuleForm (U Module
                              ;;Module*
                              ))
(define-predicate SubModuleForm? SubModuleForm)

(define-type GeneralTopLevelForm (U Expr
                                    DefineValues
                                    ;; DefineSyntaxes
                                    Require))
(define-predicate GeneralTopLevelForm? GeneralTopLevelForm)

;;;

(define-type Args      (Listof Symbol))      ;;; FIXME: this doesn't match with formals in grammar
(define-type Binding   (Pairof Args Expr))

;;; Expressions 

(define-type Begin      (Listof TopLevelForm))
(define-type CaseLambda (Listof (Pairof Args (Listof Expr))))
(struct PlainLambda     ([args : Args] [exprs : (Listof Expr)]) #:transparent)
(struct If              ([pred : Expr] [t-branch : Expr] [f-branch : Expr]) #:transparent)
(struct LetValues       ([bindings : (Listof Binding)] [body : (Listof Expr)]) #:transparent)
(struct LetRecValues    ([bindings : (Listof Binding)] [body : (Listof Expr)]) #:transparent)
(struct Set!            ([id : Symbol] [expr : Expr]) #:transparent)
(struct Quote           ([datum : Any]) #:transparent)
(struct PlainApp        ([lam : Expr] [args : (Listof Expr)]) #:transparent) ;; Special case of (PlainApp '()) produces '()
(struct TopId           ([id : Symbol]) #:transparent)
(struct VarRef          ([var : (Option (U Symbol TopId))]) #:transparent)

(define-predicate Begin? Begin)

;;; Top Level Forms

(struct Module ([id : (U Symbol String)]
                [path : (Option Path)]
                [lang : (U Symbol String)]
                [forms : (Listof ModuleLevelForm)])
  #:transparent) ;; FIXME: path

;;; Module Level Forms

(struct Provide ([id : Symbol]) #:transparent) ;; This more than just one field

;; GeneralTopLevelForm

(struct DefineValues   ([ids : Args] [expr : Expr]) #:transparent)
(struct Require        ([id : Symbol]) #:transparent) ;; This more than just one field
