#lang typed/racket/base

(require racket/match
         racket/list
         "absyn.rkt"
         "util.rkt")

(provide rename-program
         rename-top-level-form
         rename-expr)

(define-type RenameMap (HashTable Symbol Symbol))

(: rename-program (-> Program Program))
(define (rename-program p)
  (define-values (f _) (rename-top-level-form p (hash)))
  f)

(: rename-top-level-form (-> TopLevelForm RenameMap (Values TopLevelForm RenameMap)))
(define (rename-top-level-form p symap)
  (cond
    [(Expr? p) (values (rename-expr p symap) symap)]
    [(GeneralTopLevelForm? p) (rename-general-top-level-form p symap)]
    [(Module? p) (values (rename-module p symap) symap)]
    [(Begin? p) (values (rename-begin p symap) symap)]))

(: rename-general-top-level-form (-> GeneralTopLevelForm RenameMap
                                     (Values GeneralTopLevelForm RenameMap)))
(define (rename-general-top-level-form form symap)
  (cond
    [(Expr? form) (values (rename-expr form symap) symap)]
    [(DefineValues? form)
     (match-define (DefineValues ids expr) form)
     (define new-symap (fresh-symap symap ids))
     (define new-ids (map (λ ([a : Symbol])
                            (hash-ref new-symap a))
                          ids))
     ;; we have to add current ids to maps for recursive calls
     (values (DefineValues new-ids (rename-expr expr new-symap))
             new-symap)]
    [(Require? form) (values form symap)]))    ;;;; TODO

(: rename-module-level-form (-> ModuleLevelForm RenameMap (Values ModuleLevelForm RenameMap)))
(define (rename-module-level-form form symap)
  (cond
    [(GeneralTopLevelForm? form) (rename-general-top-level-form form symap)]
    [(Provide? form) (values form symap)] ;;;; TODO
    [(SubModuleForm? form) (values form symap)])) ;;;; TODO)

(: rename-module (-> Module RenameMap Module))
(define (rename-module mod symap)
  (let loop ([f (Module-forms mod)]
             [s symap]
             [fr : (Listof ModuleLevelForm) '()])
    (cond
      [(empty? f) (Module (Module-id mod)
                          (Module-path mod)
                          (Module-lang mod)
                          (reverse fr))]
      [else
       (define-values (fn sn) (rename-module-level-form (car f) s))
       (loop (rest f)
             sn
             (cons fn fr))])))

(: rename-begin (-> Begin RenameMap Begin))
(define (rename-begin forms symap)
  (let loop ([f : Begin forms]
             [fr : Begin '()]
             [symap* : RenameMap symap])
    (match f
      [(cons hd tl)
       (define-values (f s) (rename-top-level-form hd symap*))
       (loop tl (cons f fr) s)]
      [_ (reverse fr)])))

(: rename-expr (-> Expr RenameMap Expr))
(define (rename-expr expr symap)
  (match expr
    [(PlainLambda args exprs)
     (define new-bindings (fresh-symap symap args))
     (PlainLambda (map (λ ([a : Symbol])
                         (hash-ref new-bindings a)) args)
                  (map (λ ([e : Expr])
                         (rename-expr e new-bindings))
                       exprs))]
    [(If expr t-branch f-branch)
     (If (rename-expr expr symap)
         (rename-expr t-branch symap)
         (rename-expr f-branch symap))]
    [(LetValues bindings body)
     (define names (map (λ ([b : Binding]) (car b)) bindings))
     (define renames (map fresh-names names))
     (define renamed-binding-exprs
       (map (λ ([b : Binding])
              (rename-expr (cdr b) symap))
            bindings))
     (define new-binding-pairs
       (map (inst cons Symbol Symbol)
            (flatten1 names) (flatten1 renames)))
     (define new-bindings (hash-set-pair* symap new-binding-pairs))
     (LetValues (map (λ ([a : Args] [b : Expr])
                       (cons a b))
                     renames renamed-binding-exprs)
                (map (λ ([e : Expr])
                       (rename-expr e new-bindings))
                     body))]
    [(Set! id expr) (Set! (hash-ref symap id)
                          (rename-expr expr symap))]
    [(PlainApp lam args)
     (PlainApp (rename-expr lam symap)
               (map (λ ([e : Expr])
                      (rename-expr e symap))
                    args))]
    [(TopId id) expr] ;; FIXME: rename top-levels?
    [(Quote datum) expr]
    [(cons hd tl) (rename-begin expr symap)]
    [_ #:when (symbol? expr) (hash-ref symap expr)]
    [_ (error "unsupported expr" expr)]))

(: fresh-symap (-> RenameMap Args RenameMap))
(define (fresh-symap symap args)
  (let loop ([as args] [b symap])
    (match as
      [(cons hd tl) (loop tl (hash-set b hd (fresh-id hd)))]
      ['() b])))

(: fresh-names (-> Args Args))
(define (fresh-names args)
  (map fresh-id args))