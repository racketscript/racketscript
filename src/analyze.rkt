#lang typed/racket

(require "absyn.rkt"
         "util.rkt")

(provide rename-program
         rename-expr-bindings)

(define-type RenameMap (HashTable Symbol Symbol))

(: rename-program (-> Program RenameMap Program))
(define (rename-program p symap)
  (cond
    [(Expr? p) (rename-expr-bindings p symap)]
    [(GeneralTopLevelForm? p) (rename-general-top-level-form p symap)]
    [(Module? p) (rename-module p symap)]
    [(Begin? p) (rename-begin p symap)]))

(: rename-general-top-level-form (-> GeneralTopLevelForm RenameMap GeneralTopLevelForm))
(define (rename-general-top-level-form form symap)
  (cond
    [(Expr? form) (rename-expr-bindings form symap)]
    [(DefineValues? form) form] ;;;; TODO: URGENT!!!!
    [(Require? form) form]))    ;;;; TODO

(: rename-module-level-form (-> ModuleLevelForm RenameMap ModuleLevelForm))
(define (rename-module-level-form form symap)
  (cond
    [(GeneralTopLevelForm? form) (rename-general-top-level-form form symap)]
    [(Provide? form) form] ;;;; TODO
    [(SubModuleForm? form) form])) ;;;; TODO)

(: rename-module (-> Module RenameMap Module))
(define (rename-module mod symap)
  (match-define (Module id path forms) mod)
  (Module id
          path
          (map (λ ([f : ModuleLevelForm])
                 (rename-module-level-form f symap))
               forms)))

(define (rename-begin beg symap)
  beg)

(: rename-expr-bindings (-> Expr RenameMap Expr))
(define (rename-expr-bindings expr binding-map)
  (match expr
    [(PlainLambda args exprs)
     (define new-bindings (fresh-binding-map binding-map args))
     (PlainLambda (map (λ ([a : Symbol])
                         (hash-ref new-bindings a)) args)
                  (map (λ ([e : Expr])
                         (rename-expr-bindings e new-bindings))
                       exprs))]
    [(If expr t-branch f-branch)
     (If (rename-expr-bindings expr binding-map)
         (rename-expr-bindings t-branch binding-map)
         (rename-expr-bindings f-branch binding-map))]
    [(LetValues bindings body)
     (define names (map (λ ([b : Binding]) (car b)) bindings))
     (define renames (map fresh-names names))
     (define renamed-binding-exprs
       (map (λ ([b : Binding])
              (rename-expr-bindings (cdr b) binding-map))
            bindings))
     (define new-binding-pairs
       (map (inst cons Symbol Symbol)
            (flatten1 names) (flatten1 renames)))
     (define new-bindings (hash-set-pair* binding-map new-binding-pairs))
     (LetValues (map (λ ([a : Args] [b : Expr])
                       (cons a b))
                     renames renamed-binding-exprs)
                (map (λ ([e : Expr])
                       (rename-expr-bindings e new-bindings))
                     body))]
    [(Set! id expr) expr (Set! (hash-ref binding-map id)
                               (rename-expr-bindings expr binding-map))]
    [(PlainApp lam args)
     (PlainApp (rename-expr-bindings lam binding-map)
               (map (λ ([e : Expr])
                      (rename-expr-bindings e binding-map))
                    args))]
    [(TopId id) expr] ;; FIXME: rename top-levels?
    [(Quote datum) expr]
    [_ #:when (symbol? expr) (hash-ref binding-map expr)]
    [_ (error "unsupported expr")]))

(: fresh-binding-map (-> RenameMap Args RenameMap))
(define (fresh-binding-map binding-map args)
  (let loop ([as args] [b binding-map])
    (match as
      [(cons hd tl) (loop tl (hash-set b hd (gensym hd)))]
      ['() b])))

(: fresh-names (-> Args Args))
(define (fresh-names args)
  (map gensym args))
