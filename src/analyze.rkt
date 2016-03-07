#lang typed/racket

(require "absyn.rkt"
         "util.rkt")

(provide rename-expr-bindings)

(define-type RenameMap (HashTable Symbol Symbol))

(: fresh-binding-map (-> RenameMap Args RenameMap))
(define (fresh-binding-map binding-map args)
  (let loop ([as args] [b binding-map])
    (match as
      [(cons hd tl) (loop tl (hash-set b hd (gensym hd)))]
      ['() b])))

(: fresh-names (-> Args Args))
(define (fresh-names args)
  (map gensym args))

(: make-symbol-pair (-> Symbol Symbol (Pairof Symbol Symbol)))
(define (make-symbol-pair a b)
  ((inst cons Symbol Symbol) a b))

(: rename-expr-bindings (-> Expr RenameMap Expr))
(define (rename-expr-bindings expr binding-map)
  (match expr
    [(PlainLambda args exprs)
     (define new-bindings (fresh-binding-map binding-map args))
     (PlainLambda (map (λ ([a : Symbol])
                         (hash-ref new-bindings a)) args)
                  exprs)]
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
