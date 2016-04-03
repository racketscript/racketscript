#lang typed/racket/base

;;; Generate IL code from abstract syntax. Each binding name
;;; in assumed to be fresh, to enforce lexical scope rules of Racket

(require racket/match
         racket/function
         racket/list
         racket/format
         "config.rkt"
         "util.rkt"
         "absyn.rkt"
         "il.rkt")

(provide absyn-top-level->il
         absyn-gtl-form->il
         absyn-expr->il
         absyn-module->il)

(: absyn-top-level->il (-> TopLevelForm ILStatement*))
(define (absyn-top-level->il form)
  (cond
    [(Module? form) (list (absyn-module->il form))]
    [(Expr? form)
     (define-values (stmt v) (absyn-expr->il form))
     (append1 stmt v)]
    [(Begin? form) (absyn-expr->il form)]
    [else (error "only modules supported at top level")]))

(: absyn-module->il (-> Module ILModule))
(define (absyn-module->il mod)
  (: provides (Boxof (Listof ILProvide)))
  (define provides (box '()))

  (: add-provides! (-> (Listof ILProvide) Void))
  (define (add-provides! p*)
    (set-box! provides (append (unbox provides) p*)))
  
  (match-define (Module id path lang forms) mod)
  (define mod-stms
    (append-map
     (λ ([form : ModuleLevelForm])
       (cond
         [(GeneralTopLevelForm? form) (absyn-gtl-form->il form)]
         [(Provide*? form) (add-provides! (absyn-provide->il form)) '()]
         [(SubModuleForm? form) '()])) ;; TODO
     forms))
  (ILModule id (unbox provides) '() mod-stms))

(: absyn-gtl-form->il (-> GeneralTopLevelForm ILStatement*))
(define (absyn-gtl-form->il form)
  (cond
    [(Expr? form)
     (define-values (stms v) (absyn-expr->il form))
     (append1 stms v)]
    [(DefineValues? form)
     (match-define (DefineValues ids expr) form)
     (absyn-binding->il (cons ids expr))]
    [(Require*? form) '()]))

(: absyn-provide->il (-> Provide* (Listof ILProvide)))
(define (absyn-provide->il form)
  (map (λ ([f : Provide]) (ILProvide (Provide-id f))) form))
     

(: absyn-expr->il (-> Expr (Values ILStatement* ILExpr)))
;;; An expression in Racket may need to be split into several
;;; statements in JS. However, since expression always has a
;;; values, we return pair of statements and the final value
;;; of expression.
;;; TODO: returned ILExpr should be just ILValue?
(define (absyn-expr->il expr)
  (match expr
    [(PlainLambda args body)
     (define-values (body-stms body-value)
       (for/fold/last ([stms : ILStatement* '()]
                       [rv : ILExpr (ILValue (void))])
                      ([e last? body])
         (define-values (s v) (absyn-expr->il e))
         (if last?
             (values (append stms s) v)
             (values (append stms s (list v)) v))))
     (values '()
             (ILLambda args
                       (append1 body-stms (ILReturn body-value))))]
    [(If pred-e t-branch f-branch)
     (define-values (ps pe) (absyn-expr->il pred-e))
     (define-values (ts te) (absyn-expr->il t-branch))
     (define-values (fs fe) (absyn-expr->il f-branch))
     (define result-id (fresh-id 'if_res))
     (values (append ps
                     (list (ILIf pe
                                 (append1 ts (ILVarDec result-id te))
                                 (append1 fs (ILVarDec result-id fe)))))
             result-id)]
    [(LetValues bindings body)
     (define binding-stms
       (for/fold ([stms : ILStatement* '()])
                 ([b bindings])
         (append stms
                 (absyn-binding->il b))))
     (for/fold/last ([stms binding-stms]
                     [rv : ILExpr (ILValue (void))])
                    ([e last? body])
       (define-values (s nv) (absyn-expr->il e))
       (if last?
           (values (append stms s) nv)
           (values (append stms s (list nv)) nv)))]
    [(LetRecValues bindings body)
     ;; FIXME: body same as LetValues, however using 'or' on pattern failed type checker
     (define binding-stms
       (for/fold ([stms : ILStatement* '()])
                 ([b bindings])
         (append stms
                 (absyn-binding->il b))))
     (for/fold/last ([stms binding-stms]
                     [rv : ILExpr (ILValue (void))])
                    ([e last? body])
       (define-values (s nv) (absyn-expr->il e))
       (if last?
           (values (append stms s) nv)
           (values (append stms s (list nv)) nv)))]
    [(Set! id e)
     (values (let-values ([(stms v) (absyn-expr->il e)])
               (append1 stms
                        (ILAssign id v)))
             (ILValue (void)))]
    [(PlainApp lam args)
     (let loop ([arg-stms : ILStatement* '()]      ;;; statements for computing arguments
                [arg* : (Listof ILExpr) '()]       ;;; expressions passed to the lam
                [arg args])
       (match arg
         ['()
          (cond
            [(symbol? lam) (values arg-stms
                                   (ILApp lam arg*))]
            [else (define-values (stms v) (absyn-expr->il lam))
                  (values (append arg-stms stms)
                          (ILApp v arg*))])]
         [(cons hd tl)
          (define-values (s v) (absyn-expr->il hd))
          (cond
            [(ILExpr? v)
             (loop (append arg-stms s)
                   (append1 arg* v)
                   tl)]
            [else
             (define temp-id (fresh-id 'arg-temp))
             (loop (append arg-stms s (list (ILVarDec temp-id v)))
                   (append1 arg* temp-id)
                   tl)])]))]
    [(TopId id) (values '() id)] ;; FIXME: rename top-levels?
    [(Quote datum) (values '() (absyn-value->il datum))]
    ;; Begin Statements
    [(cons hd '()) (cond
                     [(Expr? hd) (absyn-expr->il hd)]
                     [else (error "last datum in body must be expression")])]
    [(cons hd tl)
     (define hd-stms (absyn-top-level->il hd))
     (define-values (tl-stms v) (absyn-expr->il tl))
     (values (append hd-stms tl-stms)
             v)]
    [_ #:when (symbol? expr) (values '() expr)]
    [_ (error (~a "unsupported expr " expr))]))

(: absyn-binding->il (-> Binding ILStatement*))
(define (absyn-binding->il b)
  (match-define (cons args expr) b)
  (define-values (stms v) (absyn-expr->il expr))
  (match args
    [(list a)
     (append1 stms
              (ILVarDec a v))]
    [_
     (define result-id (fresh-id 'let_result))
     (define binding-stms
       (for/fold ([stms : ILStatement* stms])
                 ([i : Natural (range (length args))]
                  [arg : Symbol args])
         (append1 stms
                  (ILValuesMatch arg result-id i))))
     (cons (ILVarDec result-id v)
           binding-stms)]))

(: absyn-value->il (-> Any ILValue))
(define (absyn-value->il d)
  (cond
    [(or (symbol? d)
         (string? d)
         (integer? d)
         (list? d)
         (boolean? d)
         (vector? d)
         (struct? d)
         (real? d))
     (ILValue (Quote-datum d))]
    [else (error (~a "unsupported value" d))]))
