#lang typed/racket

(provide (all-defined-out))

(define-type Expr (U
                   Symbol
                   PlainLambda
                   CaseLambda
                   If
                   Begin
                   LetValues
                   LetRecValues
                   Set!
                   Quote
                   ;; QuoteSyntax
                   ;; WithContinuationMark
                   PlainApp
                   TopId
                   VarRef))

(define-type TopLevelForm (U
                           GeneralTopLevelForm
                           Expr
                           Module
                           Begin
                           ;; BeginForSyntax
                           ))

(define-type ModuleLevelForm (U
                              GeneralTopLevelForm
                              Provide
                              ;; BeginForSyntax
                              SubModuleForm
                               ;; Declare
                              ))

(define-type SubModuleForm (U
                            Module
                            ;;Module*
                            ))

(define-type GeneralTopLevelForm (U
                                  Expr
                                  DefineValues
                                  ;; DefineSyntaxes
                                  Require))

;;;

(define-type Args (Listof Symbol))
(define-type Binding (Pairof Args Expr))

;;; Expressions 

(struct PlainLambda ([args : Args] [exprs : (Listof Expr)]))

(define-type CaseLambda (Listof (Pairof Args (Listof Expr))))

(struct If ([pred : Expr] [t-branch : Expr] [f-branch : Expr]))

(define-type Begin (Listof (U TopLevelForm Expr)))


(struct LetValues ([bindings : (Listof Binding)] [body : (Listof Expr)]))

(struct LetRecValues ([bindings : (Listof Binding)] [body : (Listof Expr)]))

(struct Set! ([id : Symbol] [expr : Expr]))

(struct Quote ([datum : Datum]))

(struct PlainApp ([exprs : (Listof Expr)])) ;; Special case of (PlainApp '()) produces '()

(struct TopId ([id : Symbol]))

(struct VarRef ([var : (Option (U Symbol TopId))]))


;;; Top Level Forms

(define-type PlainModule (Listof ModuleLevelForm))

(struct Module ([id : Symbol] [path : Path] [plain-mod : PlainModule]))

;;; Module Level Forms

(struct Provide ([id : Symbol])) ;; This more than just one field

;; GeneralTopLevelForm

(struct DefineValues ([ids : Args] [expr : Expr]))

(struct Require ([id : Symbol])) ;; This more than just one field




