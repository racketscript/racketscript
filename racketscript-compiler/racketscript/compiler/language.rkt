#lang typed/racket/base

(provide define-language)

(require racket/struct
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     syntax/stx))

;; Identifier -> Identifer
;; Given an identifier `name`, return  `name?`
(define-for-syntax (make-pred id)
  (format-id id "~a?" (syntax-e id)))

;; Macro to transform a BNF styled syntax to bunch of structs and
;; union types.
(define-syntax (define-language stx)
  ;; Syntax classes we expect after #:alias in language definition.
  ;; Rather than using define-type, generated code will use
  ;; define-type-alias
  (define-syntax-class typealias
    (pattern [name:id def]))

  ;; A BNF styled definition with types. Each case (typecase) in
  ;; defintion is either a typed racket type or a compound type, for
  ;; which structs are generated. All cases are combined with union.
  ;;
  ;; Eg.
  ;;   - [Expr Number
  ;;           Symbol
  ;;           (Plus [a : Number] [b : Number]
  ;;           (Minus [a: NUmber] [b : Number])]
  (define-syntax-class typedecl
    (pattern [name:id τ-case:typecase ...]))

  ;; Represents individual cases in type declaration.
  (define-syntax-class typecase
    (pattern name:id)
    (pattern (name:id
              [field-name:id (~datum :) field-type] ...))
    (pattern name))

  ;; Maybe<Syntax> -> Syntax
  ;; Generate type definition and predicate code for creating an alias
  (define (generate-alias stx)
    (syntax-parse stx
      [(~datum #f) #`()]
      [ta:typealias
       #`((define-type-alias ta.name ta.def)
          (define-predicate #,(make-pred #`ta.name) ta.name))]))

  ;; Syntax:typecase -> (Syntax-List Syntax Syntax)
  ;;
  ;; Generate code for given type return an identifier/syntax
  ;; representing the type and code that generates this type. For
  ;; cases where the types are already definied there is no need to
  ;; generate code.
  ;;
  ;; Structures equality has to be based on object identity.
  (define (generate-typecase stx)
    (syntax-parse stx
      [name:id #`(name)]
      [(name:id [field-name:id : field-type] ...)
       #:with (struct-accessor ...) (stx-map
                                     (λ (f)
                                       (format-id #'name "~a-~a" #'name f))
                                     #`(field-name ...))
       #`(name (struct name ([field-name : field-type] ...) #:transparent))]
      [name #`(name)]))

  ;; Syntax -> Syntax
  ;;
  ;; Generate code for type and its cases.
  (define (generate-decl stx)
    (syntax-parse stx
      [Τ:typedecl
       #:with ((τ-name τ ...) ...) (stx-map generate-typecase #`(Τ.τ-case ...))
       #:with type-predicate #`(define-predicate #,(make-pred #`Τ.name) Τ.name)
       #:with type-define
       (case (length (syntax-e #`(Τ.τ-case ...)))
         [(0) (error "Atleast one case required in declaration")]
         [(1) #`(define-type Τ.name τ-name ...)]
         [else #`(define-type Τ.name (U τ-name ...))])
       #`(type-define type-predicate τ ... ...)]))

  (syntax-parse stx
    [(define-language lang-name:id
       (~or (~seq #:alias
                  α:typealias ...+)
            (~seq #:forms
                  Τ:typedecl ...+)) ...)
     #:with ((α* α-pred*) ...) (stx-map generate-alias #`(α ... ...))
     #:with ((Τ* ...) ...) (stx-map generate-decl #`(Τ ... ...))
     #`(begin α* ... α-pred* ...
              Τ* ... ...)]))
