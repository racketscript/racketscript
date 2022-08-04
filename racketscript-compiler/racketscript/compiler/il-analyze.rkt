#lang racket/base

(require racket/bool
         racket/list
         "struct-match.rkt"
         "match.rkt"
         "set.rkt"
         "environment.rkt"
         "ir.rkt"
         "ast.rkt"
         "util.rkt")

(provide self-tail->loop
         lift-returns
         insert-arity-checks
         flatten-if-else
         free-identifiers
         free+defined
         has-application?)

;; TODO: Remove dead return statements.
;; TODO: Variable number arguments.
;; TODO: Identify self calls which are not simple symbol names.
;;       Eg. ILRef
;; TODO: Identify mutually recursive calls.

(module+ test
  (require rackunit)
  (require "ir.rkt"))

;; ----------------------------------------------------------------------------
;; Add arity checking in IL

(define (check-arity-stms formals)
  (define (check-arity-stm op arity)
    (ILIf (ILBinaryOp op (list (ILRef (ILArguments) 'length) (ILValue arity)))
          (list
           (ILThrow (ILApp (name-in-module 'core 'racketContractError)
                           (list (ILValue "arity mismatch")))))
          '()))

  (define formals* (ILCheckedFormals-formals formals))
  (cond
    [(symbol? formals*) '()]
    [(list? formals*)
     (list (check-arity-stm '!== (length formals*)))]
    [(cons? formals*)
     (define pos-formals (car formals*))
     (list (check-arity-stm '< (length pos-formals)))]))

(define (insert-arity-checks mod)
  (define traverse-expr
    (struct-match-lambda
      [(ILLambda args body)
       (define-values (new-args check-stms)
         (struct-match args
           [(ILCheckedFormals args*)
            (values args*
                    (check-arity-stms args))]

           [v (values v '())]))
       (ILLambda new-args
                 (append check-stms (traverse-stm* body)))]
      [(ILApp lam args)
       (ILApp (traverse-expr lam) (map traverse-expr args))]
      [(ILBinaryOp oper args) (ILBinaryOp oper (map traverse-expr args))]
      [(ILArray items) (ILArray (map traverse-expr items))]
      [(ILObject items) (ILObject (map (λ (o)
                                         (cons (car o)
                                               (traverse-expr (cdr o))))
                                       items))]
      [(ILRef expr fieldname)
       (ILRef (traverse-expr expr) fieldname)]
      [(ILIndex expr field-expr)
       (ILIndex (traverse-expr expr) (traverse-expr field-expr))]
      [(ILNew e) (ILNew (traverse-expr e))]
      [(ILInstanceOf expr type)
       (ILInstanceOf (traverse-expr expr) (traverse-expr type))]
      [(ILTypeOf expr) (ILTypeOf (traverse-expr expr))]
      [v v]))

  (define traverse-stm
    (struct-match-lambda
      [(ILVarDec id expr)
       (ILVarDec id (if expr
                        (traverse-expr expr)
                        #f))]
      [(ILLetDec id expr)
       (ILLetDec id (if expr
                        (traverse-expr expr)
                        #f))]
      [(ILIf pred t-branch f-branch)
       (ILIf (traverse-expr pred)
             (traverse-stm* t-branch)
             (traverse-stm* f-branch))]
      [(ILIf* clauses)
       (ILIf* (for/list ([c clauses])
                (define pred (IfClause-pred c))
                (define body (IfClause-body c))
                (IfClause (and pred (traverse-expr pred))
                          (traverse-stm* body))))]
      [(ILAssign lvalue rvalue)
       (ILAssign (traverse-expr lvalue)
                 (traverse-expr rvalue))]
      [(ILWhile condition body)
       (ILWhile (traverse-expr condition)
                (traverse-stm* body))]
      [(ILExnHandler try error catch finally)
       (ILExnHandler (traverse-stm* try)
                     error
                     (traverse-stm* catch)
                     (traverse-stm* finally))]
      [(ILReturn expr) (ILReturn (traverse-expr expr))]
      [(ILThrow expr) (ILThrow (traverse-expr expr))]
      [(? ILExpr? expr) (traverse-expr expr)]
      [v v]))

  (define (traverse-stm* stm*)
    (map traverse-stm stm*))

  (struct-match mod
    [(ILLinklet importss exports body)
     (ILLinklet importss exports (traverse-stm* body))]))

;; ----------------------------------------------------------------------------
;; Tail Call Optimization

;; Frontend function to convert self tail calls to
;; loops.
(define (self-tail->loop il*)
  (x-self-tail->loop (lift-returns il*)))

;; Lifts return statements up by one statement, if
;; the identifier returned, was declared or re-assigned
;; in previous statement.
(define (lift-returns il*)
  ;; Track all variables delcared in current function scope.
  (define current-scope-declarations
    (make-parameter (set)))

  ;; Keep a list of variable declarations whose next runtime statement
  ;; is returning value held by same variable. We check this set for each
  ;; ILReturn statement and decide if it should be removed.
  (define removed-declarations (make-parameter (set)))

  ;; Next statement that will be executed after
  ;; the statement we are currently handling.
  (define next-statement (make-parameter #f))

  ;; Adds sym to scope of declared variables.
  (define (add-to-scope! sym)
    (current-scope-declarations
     (set-add (current-scope-declarations) sym)))

  ;; Returns true if we have discovered `sym`
  (define (var-in-scope? sym)
    (set-member? (current-scope-declarations) sym))

  (define (handle-expr e)
    (struct-match e
      [(ILLambda args body)
       (parameterize ([current-scope-declarations (set)]
                      [next-statement #f])
         ;; TODO: add parameters to environment
         (ILLambda args (handle-stm* body)))]
      [(ILBinaryOp operator operands)
       (ILBinaryOp operator
                   (map handle-expr operands))]
      [(ILApp lam args)
       (ILApp (handle-expr lam)
              (map handle-expr args))]
      [(ILArray items)
       (ILArray (map handle-expr items))]
      [(ILObject items)
       (ILObject (map (λ (i)
                        (cons (car i)
                              (handle-expr (cdr i))))
                      items))]
      [(ILRef expr fieldname)
       (ILRef (handle-expr expr) fieldname)]
      [(ILIndex expr fieldexpr)
       (ILIndex (handle-expr expr) (handle-expr fieldexpr))]
      [(ILInstanceOf expr type)
       (ILInstanceOf (handle-expr expr) (handle-expr type))]
      [(ILTypeOf expr)
       (ILTypeOf (handle-expr expr))]
      [(ILNew v)
       (ILNew (handle-expr v))]
      [(ILValue v) e]
      [(ILUndefined) e]
      [(ILArguments) e]
      [(ILThis) e]
      [(ILNull) e]
      [(? symbol? v) v]))

  (define (handle-stm stm)
    (struct-match stm
      [(ILVarDec id expr)
       (struct-match (next-statement)
         [(ILReturn e) #:when (and expr (equal? e id))
          (removed-declarations (set-add (removed-declarations) id))
          (ILReturn (handle-expr expr))]
         [_ (add-to-scope! id)
            (ILVarDec id (and expr
                              (handle-expr expr)))])]
      [(ILLetDec id expr)
       ;; TODO: `id` is block scope, while we are only tracking
       ;; function scope.
       (ILLetDec id (and expr (handle-expr expr)))]
      [(ILAssign lvalue rvalue)
       (struct-match (next-statement)
         [(ILReturn e) #:when (and (symbol? lvalue)
                                   (var-in-scope? lvalue)
                                   (equal? e lvalue))
          (ILReturn (handle-expr rvalue))]
         [_ (ILAssign (handle-expr lvalue)
                      (handle-expr rvalue))])]
      [(ILIf pred t-branch f-branch)
       (match-define `(,t-branch* ,t-removed)
         (parameterize ([removed-declarations (removed-declarations)])
           (list (handle-stm* t-branch)
                 (removed-declarations))))
       (match-define `(,f-branch* ,f-removed)
         (parameterize ([removed-declarations (removed-declarations)])
           (list (handle-stm* f-branch)
                 (removed-declarations))))
       (removed-declarations (set-intersect t-removed f-removed))
       (ILIf (handle-expr pred) t-branch* f-branch*)]
      [(ILIf* clauses)
       (define-values (clauses* removed)
         (for/fold ([c*      '()]
                    [removed '()])
                   ([clause (reverse clauses)])
           (let* ([pred (ILIfClause-pred clause)]
                  [body (ILIfClause-body clause)]
                  [pred* (and pred (handle-expr pred))])
             (parameterize ([removed-declarations (removed-declarations)])
               (values (cons (ILIfClause pred* (handle-stm* body)) c*)
                       (cons (removed-declarations) removed))))))
       (removed-declarations (match removed
                               [`(,hd) hd]
                               [`(,hd0 . ,tl)
                                (foldl set-intersect
                                       hd0
                                       tl)]))
       (ILIf* clauses*)]
      [(ILWhile condition body)
       (ILWhile (handle-expr condition)
                (handle-stm* body))]
      [(ILExnHandler try-block error catch-block finally-block)
       ;; We ignore TCO for this to make with-continuation-mark behave
       ;; better. The context gets out of context as we just loop and
       ;; get out of try/catch/finally block
       #;(match-define (list try-block* try-removed)
         (parameterize ([removed-declarations (removed-declarations)])
           (list (handle-stm* try-block)
                 (removed-declarations))))
       #;(match-define (list catch-block* catch-removed)
         (parameterize ([removed-declarations (removed-declarations)])
           (list (handle-stm* catch-block)
                 (removed-declarations))))
       #;(match-define (list finally-block* finally-removed)
         (parameterize ([removed-declarations (removed-declarations)])
           (list (handle-stm* finally-block)
                 (removed-declarations))))
       #;(removed-declarations (set-intersect try-removed
                                            catch-removed
                                            finally-removed))
       #;(ILExnHandler try-block* error catch-block* finally-block*)
       (ILExnHandler try-block error catch-block finally-block)]
      [(ILThrow expr)
       (ILThrow (handle-expr expr))]
      [(ILReturn expr)
       (if (and (symbol? expr)
                (set-member? (removed-declarations) expr))
           '()
           (ILReturn (handle-expr expr)))]
      [(ILLabel _) stm]
      [(ILContinue _) stm]
      [(? ILExpr? expr) (handle-expr expr)]))

  (define (handle-stm* stm*)
    (let loop ([result '()]
               [stm* stm*])
      (match stm*
        [`() result]
        [`(,hd . ,tl)
         (define next (if (empty? tl)
                          (next-statement)
                          (car tl)))
         (define stm**
           (if (empty? tl)
               (handle-stm hd)
               (parameterize ([next-statement (car tl)])
                 (handle-stm hd))))

         (loop (append result (if (list? stm**) stm** (list stm**))) tl)])))
           

         ;; (match stm**
         ;;   [(? ILStatement? stm)
         ;;    (loop (append result (list stm)) tl)]
         ;;   [(? ILStatement*? stms)
         ;;    (loop (append result stms) tl)])

  ;; Try lifting until convergence
  (let ([result (handle-stm* il*)])
    (if (equal? result il*)
        result
        (lift-returns result))))

[module+ test
  (check-equal?
   (lift-returns
    (list
     (ILVarDec
      'fact
      (ILLambda
       '(n2 a3)
       (list
        (ILIf
         (ILApp 'zero? '(n2))
         (list (ILVarDec 'if_res2 'a3))
         (list
          (ILVarDec
           'if_res2
           (ILApp
            'fact
            (list (ILApp 'sub1 '(n2)) (ILBinaryOp '* '(n2 a3)))))))
        (ILReturn 'if_res2))))))
   (list
    (ILVarDec
     'fact
     (ILLambda
      '(n2 a3)
      (list
       (ILIf
        (ILApp 'zero? '(n2))
        (list (ILReturn 'a3))
        (list
         (ILReturn
          (ILApp 'fact (list (ILApp 'sub1 '(n2))
                             (ILBinaryOp '* '(n2 a3)))))))))))
   "Lift return statements up and remove the unreachable return.")

  (check-equal?
   (lift-returns
    (list
     (ILVarDec
      'fact
      (ILLambda
       '(n2 a3)
       (list
        (ILIf
         (ILApp 'zero? '(n2))
         (list
          (ILVarDec
           'if_res2
           (ILBinaryOp '+ (list (ILValue 1)
                                (ILApp 'fact '(a3)))))
          (ILVarDec 'foo 'a3))
         (list
          (ILVarDec
           'if_res2
           (ILApp
            'fact
            (list (ILApp 'sub1 '(n2)) (ILBinaryOp '* '(n2 a3)))))))
        (ILReturn 'if_res2))))))
   (list
    (ILVarDec
     'fact
     (ILLambda
      '(n2 a3)
      (list
       (ILIf
        (ILApp 'zero? '(n2))
        (list
         (ILVarDec
          'if_res2
          (ILBinaryOp '+ (list (ILValue 1) (ILApp 'fact '(a3)))))
         (ILVarDec 'foo 'a3))
        (list
         (ILReturn
          (ILApp 'fact (list (ILApp 'sub1 '(n2)) (ILBinaryOp '* '(n2 a3)))))))
       (ILReturn 'if_res2)))))
   "Lift return statements up but don't remove trailing return.")

  (check-equal?
   (lift-returns
    (list
     (ILVarDec
      'fact
      (ILLambda
       '(n2 a3)
       (list
        (ILIf
         (ILApp 'zero? '(n2))
         (list
          (ILVarDec
           'if_res2
           (ILApp
            'fact
            (list (ILApp 'sub1 '(n2)) (ILBinaryOp '* '(n2 a3))))))
         (list
          (ILVarDec
           'if_res2
           (ILBinaryOp '+ (list (ILValue 1)
                                (ILApp 'fact '(a3)))))
          (ILVarDec 'foo 'a3)))
        (ILReturn 'if_res2))))))
   (list
    (ILVarDec
     'fact
     (ILLambda
      '(n2 a3)
      (list
       (ILIf
        (ILApp 'zero? '(n2))
        (list
         (ILReturn
          (ILApp
           'fact
           (list (ILApp 'sub1 '(n2)) (ILBinaryOp '* '(n2 a3))))))
        (list
         (ILVarDec
          'if_res2
          (ILBinaryOp '+ (list (ILValue 1)
                               (ILApp 'fact '(a3)))))
         (ILVarDec 'foo 'a3)))
       (ILReturn 'if_res2)))))
   "Lift return statements up but don't remove trailing return.")

  (check-equal?
   (lift-returns
    (list
     (ILLambda
      '()
      (list
       (ILVarDec 'a (ILValue 0))
       (ILAssign 'a (ILValue 1))
       (ILVarDec 'b (ILValue 2))
       (ILReturn 'a)))))
   (list
    (ILLambda
     '()
     (list
      (ILVarDec 'a (ILValue 0))
      (ILAssign 'a (ILValue 1))
      (ILVarDec 'b (ILValue 2))
      (ILReturn 'a))))
   "Can't lift return statements.")

  (check-equal?
   (lift-returns
    (list
     (ILLambda
      '()
      (list
       (ILVarDec 'a (ILValue 0))
       (ILAssign 'a (ILValue 1))
       (ILReturn 'a)))))
   (list
    (ILLambda
     '()
     (list
      (ILVarDec 'a (ILValue 0))
      (ILReturn (ILValue 1))
      (ILReturn 'a))))
   "Lift last return statement.")

  ;; function foo() {
  ;;   if (...) {
  ;;      if (...) {
  ;;         var result = foo();
  ;;      } else {
  ;;         var result = 0;
  ;;      }
  ;;      return result;
  ;;   } else {
  ;;      return 0;
  ;;   }
  (check-equal?
   (lift-returns
    (list
     (ILLambda
      '()
      (list
       (ILIf 'predicate
             (list
              (ILIf 'predicate2
                    (list (ILVarDec 'result (ILApp 'foo '())))
                    (list (ILVarDec 'result (ILValue 0))))
              (ILReturn 'result))
             (list
              (ILReturn (ILValue 0))))))))
   (list
    (ILLambda
     '()
     (list
      (ILIf 'predicate
            (list (ILIf 'predicate2
                        (list (ILReturn (ILApp 'foo '())))
                        (list (ILReturn (ILValue 0)))))
            (list (ILReturn (ILValue 0)))))))
   "Lift returns at deeper nesting")

  (check-equal?
   (lift-returns
    (list
     (ILLambda '()
               (list
                (ILVarDec 'a (ILValue 0))
                (ILVarDec 'b 'a)
                (ILVarDec 'c 'b)
                (ILVarDec 'd 'c)
                (ILReturn 'd)))))
   (list
    (ILLambda '()
              (list (ILReturn (ILValue 0)))))
   "Lift return multiple times")

  (check-equal?
   (lift-returns
    (list (ILLambda '()
                    (list
                     (ILLetDec 'a (ILValue 0))
                     (ILIf (ILValue #t)
                           (list (ILLetDec 'a (ILValue 1)))
                           (list (ILLetDec 'a (ILValue 2))))
                     (ILReturn 'a)))))
    (list (ILLambda '()
                    (list
                     (ILLetDec 'a (ILValue 0))
                     (ILIf (ILValue #t)
                           (list (ILLetDec 'a (ILValue 1)))
                           (list (ILLetDec 'a (ILValue 2))))
                     (ILReturn 'a))))
    "Differntiate between let declaration used in different scope")

  ;; TODO
  #;(check-equal?
   (lift-returns
    (list (ILLambda '()
                    (list
                     (ILLetDec 'a (ILValue 0))
                     (ILIf (ILValue #t)
                           (list (ILLetDec 'a (ILValue 1))
                                 (ILReturn 'a))
                           (list (ILLetDec 'a (ILValue 2))
                                 (ILReturn 'a)))))))
   (list (ILLambda '()
                    (list
                     (ILLetDec 'a (ILValue 0))
                     (ILIf (ILValue #t)
                           (list (ILReturn (ILValue 1)))
                           (list (ILReturn (ILValue 2)))))))
   "Lift returns for let decs in same scope.")

  (check-equal?
   (lift-returns
    (list
     (ILIf* (list
             (ILIfClause (ILValue 1) (list (ILVarDec 'return-val (ILValue 1))))
             (ILIfClause (ILValue 2) (list (ILVarDec 'return-val (ILValue 2))))
             (ILIfClause #f (list (ILVarDec 'return-val (ILValue #f))))))
     (ILReturn 'return-val)))
   (list
    (ILIf* (list (ILIfClause (ILValue 1) (list (ILReturn (ILValue 1))))
                 (ILIfClause (ILValue 2) (list (ILReturn (ILValue 2))))
                 (ILIfClause #f (list (ILReturn (ILValue #f)))))))
   "Lift return in if-else-if-else statements")]


;; Translate self tail calls to loop. The given `il`
;; is assumed to have gone through return lifting.
(define (x-self-tail->loop il)
  (define lambda-name (make-parameter #f))
  (define lambda-formals (make-parameter '()))

  ;; If we apply TCO, we will change the original lambda formal names,
  ;; and inside the loop, use `let` statment to bind the original
  ;; formal names with values, making closures act sanely
  (define lambda-updated-formals (make-parameter #f))

  (define lambda-start-label (make-parameter #f))

  (define (reset-formals original-formals new-formals)
    (cond
      [(and (ILCheckedFormals? original-formals)
            (ILCheckedFormals? new-formals))
       (struct-match-define (ILCheckedFormals original-formals*) original-formals)
       (struct-match-define (ILCheckedFormals new-formals*) new-formals)
       (reset-formals original-formals* new-formals*)]
      [(and (symbol? original-formals) (symbol? new-formals))
       (list (ILLetDec original-formals new-formals))]
      [(and (list? original-formals) (list? new-formals))
       (map ILLetDec original-formals new-formals)]
      [(and (pair? original-formals) (pair? new-formals))
       (match-define `((,original-formals-i . ,original-formals-r) .
                       (,new-formals-i . ,new-formals-r))
         (cons original-formals new-formals))
       (append (reset-formals original-formals-i new-formals-i)
               (reset-formals original-formals-r new-formals-r))]
      [else (error 'reset-formals "Incompatible formals: ~a vs ~a" original-formals new-formals)]))

  (define (handle-expr/general e) (handle-expr e #f #f))

  (define (handle-expr e tail-position? vardec)
    (struct-match e
      [(ILLambda args body)
       (parameterize ([lambda-name vardec]
                      [lambda-formals args]
                      [lambda-start-label (fresh-id 'lambda-start)]
                      [lambda-updated-formals #f])
         (define body*
           (let ([s* (handle-stm* body)]
                 [new-frmls (lambda-updated-formals)])
             (cond
               [(false? new-frmls) s*]
               [else
                (define reset-formals-decls
                  ;; Let is used as, it has block level scope, as
                  ;; opposed to var which has function level scope,
                  ;; which breaks the closure in case any of the
                  ;; closure variable is mutated.
                  (reset-formals (lambda-formals) new-frmls))
                (list (ILLabel (lambda-start-label))
                      (ILWhile (ILValue #t)
                               (append reset-formals-decls s*)))])))
         (ILLambda (or (lambda-updated-formals) args)
                   body*))]
      [(ILApp lam args)
       (ILApp lam (map handle-expr/general args))]
      [(ILBinaryOp oper args)
       (ILBinaryOp oper (map handle-expr/general args))]
      [(ILArray items) (ILArray (map handle-expr/general items))]
      [(ILObject items) (ILObject
                         (map (λ (item)
                                (cons (car item)
                                      (handle-expr/general (cdr item))))
                              items))]
      [(ILRef expr fieldname) (ILRef (handle-expr/general expr) fieldname)]
      [(ILIndex expr fieldname) (ILIndex (handle-expr/general expr)
                                         (handle-expr/general fieldname))]
      [(ILInstanceOf expr type)
       (ILInstanceOf (handle-expr/general expr)
                     (handle-expr/general type))]
      [(ILTypeOf expr)
       (ILTypeOf (handle-expr/general expr))]
      [(ILValue v) e]
      [(ILUndefined) e]
      [(ILArguments) e]
      [(ILThis) e]
      [(ILNull) e]
      [(ILNew v) e]
      [(? symbol? v) e]))

  (define (handle-stm s)
    (struct-match s
      [(ILReturn e) #:when (ILApp? e)
       (struct-match-define (ILApp lam args) e)
       (cond
         [(and (equal? (lambda-name) lam)
               #;(not (list? (lambda-formals))) ;; TODO: Make it work for variadict lambdas
               (il-formals-arity-includes (lambda-formals) (length args)))
          ;; Its self recursive call
          (define new-frmls
            (let ([old-updated-frmls (lambda-updated-formals)])
              (if (false? old-updated-frmls)
                  (il-freshen-formals (lambda-formals))
                  old-updated-frmls)))
          (lambda-updated-formals new-frmls)
          (define compute-args
            (for/list  ([frml (il-formals->list new-frmls)]
                        [a args])
              (ILAssign frml (handle-expr/general a))))
          (append compute-args
                  (list (ILContinue
                         (lambda-start-label))))]
         [else (ILReturn (ILApp lam (map handle-expr/general args)))])]
      [(ILVarDec id expr)
       (ILVarDec id (and expr
                         (handle-expr expr #f id)))]
      [(ILLetDec id expr)
       (ILLetDec id (and expr
                         (handle-expr expr #f id)))]
      [(ILIf pred t-branch f-branch)
       (ILIf (handle-expr/general pred)
             (handle-stm* t-branch)
             (handle-stm* f-branch))]
      [(ILIf* clauses)
       (ILIf* (map (λ (c)
                     (struct-match-define (ILIfClause pred body) c)
                     (ILIfClause (and pred (handle-expr/general pred))
                                 (handle-stm* (ILIfClause-body c))))
                   clauses))]
      [(ILAssign lvalue rvalue)
       ;; TODO: Handle ILLValue
       (if (symbol? lvalue)
           (ILAssign lvalue (handle-expr rvalue #f lvalue))
           (ILAssign lvalue (handle-expr rvalue #f #f)))]
      [(ILWhile condition body)
       (ILWhile (handle-expr/general condition)
                (handle-stm* body))]
      [(ILExnHandler try error catch finally)
       (ILExnHandler (handle-stm* try)
                     error
                     (handle-stm* catch)
                     (handle-stm* finally))]
      [(ILThrow expr)
       (ILThrow (handle-expr/general expr))]
      [(ILReturn expr) (ILReturn (handle-expr expr #t #f))]
      [(ILContinue n) s]
      [(ILLabel n) s]
      [(? ILExpr? expr) (handle-expr/general expr)]))

  (define (handle-stm* s*)
    (for/fold ([result '()])
              ([s s*])
      (define stm* (handle-stm s))
      (struct-match stm*
        [(? ILStatement? r) (append result (list r))]
        [(? ILStatement*? r*) (append result r*)])))

  (handle-stm* il))

(module+ test

  (define-syntax-rule (check-equal?* args ...)
    (parameterize ([fresh-id-counter 0])
      (check-equal? args ...)))


  (check-equal?*
   (x-self-tail->loop
    (list
     (ILVarDec
      'fact
      (ILLambda
       '(n a)
       (list
        (ILIf (ILApp 'zero? '(n))
              (list(ILReturn 'a))
              (list
               (ILReturn
                (ILApp 'fact
                       (list
                        (ILApp 'sub1 '(n))
                        (ILBinaryOp '* '(n a))))))))))))
   (list
    (ILVarDec
     'fact
     (ILLambda
      '(_n2 _a3)
      (list
       (ILLabel 'lambda-start1)
       (ILWhile
        (ILValue #t)
        (list
         (ILLetDec 'n '_n2)
         (ILLetDec 'a '_a3)
         (ILIf (ILApp 'zero? '(n))
               (list (ILReturn 'a))
               (list
                (ILAssign '_n2 (ILApp 'sub1 '(n)))
                (ILAssign '_a3 (ILBinaryOp '* '(n a)))
                (ILContinue 'lambda-start1)))))))))
   "Translate self tail recursive factorial to loops")

  (check-equal?*
   (x-self-tail->loop
    (list
     (ILVarDec
      'fact
      (ILLambda
       '(n)
       (list (ILIf
              'x
              (list (ILReturn (ILApp 'fact (list (ILApp 'sub1 '(n))))))
              (list (ILReturn (ILApp 'fact (list (ILApp 'add1 '(n))))))))))))
   (list
    (ILVarDec
     'fact
     (ILLambda
      '(_n2)
      (list
       (ILLabel 'lambda-start1)
       (ILWhile
        (ILValue #t)
        (list
         (ILLetDec 'n '_n2)
         (ILIf
          'x
          (list (ILAssign '_n2 (ILApp 'sub1 '(n))) (ILContinue 'lambda-start1))
          (list
           (ILAssign '_n2 (ILApp 'add1 '(n)))
           (ILContinue 'lambda-start1)))))))))
   "Assign to same argument names in both branches"))


;; ----------------------------------------------------------------------------
;; Compute defs, use, and free sets

(define (free+defined stms* defs)
  (define find* free+defined)

  ;; Returns (list defined-idents free-idents)
  (define (find stm defs)
    (struct-match stm
      [(ILVarDec id expr)
       (list (set id)
             (if expr
                 (second (find expr (set-add defs id)))
                 (set)))]
      [(ILLetDec id expr)
       ;;TODO: Add test
       (list (set id)
             (if expr
                 (second (find expr (set-add defs id)))
                 (set)))]
      [(ILIf pred t-branch f-branch)
       (match-define `(,_ ,p-free) (find pred defs))
       (match-define `(,t-defs ,t-free) (find* t-branch defs))
       (match-define `(,f-defs ,f-free) (find* f-branch defs))
       (list (set-union t-defs f-defs)
             (set-union p-free t-free f-free))]
      [(ILIf* clauses)
       (define-values (defs* free*)
         (for/fold ([r-defs (set)]
                    [r-free (set)])
                   ([clause clauses])
           (struct-match-define (ILIfClause pred body) clause)
           (match-define `(,_ ,p-free) (if pred
                                           (find pred defs)
                                           (list (set) (set))))
           (match-define `(,b-defs ,b-free) (find* body defs))
           (values (set-union r-defs b-defs)
                   (set-union r-free p-free b-free))))
       (list defs* free*)]
      [(ILAssign lvalue rvalue)
       (match-define `(,_ ,lv-free) (find lvalue defs))
       (match-define `(,_ ,rv-free) (find rvalue defs))
       (list (set) (set-union lv-free rv-free))]
      [(ILWhile condition body)
       (match-define `(,_ ,cond-free) (find condition defs))
       (match-define `(,body-defs ,body-free) (find* body defs))
       (list body-defs
             (set-union cond-free body-free))]
      [(ILReturn expr) (find expr defs)]
      [(ILLambda args expr)
       (define args-set (list->set (il-formals->list args)))
       (match-define `(,_ ,e-free) (find* expr (set-union defs args-set)))
       (list (set) e-free)]
      [(ILApp lam args)
       (match-define `(,_ ,l-free) (find lam defs))
       (match-define `(,_ ,a-free) (find* args defs))
       (list (set) (set-union l-free a-free))]
      [(ILBinaryOp oper args)
       (find* args defs)]
      [(ILArray items)
       (find* items defs)]
      [(ILObject items)
       ;;TODO: Check if we need to do this
       (define fields (list->set (filter symbol? (ILObject-fields stm))))
       (define bodies (ILObject-bodies stm))
       (find* bodies (set-union defs fields))]
      [(ILRef expr fieldname)
       (find expr defs)]
      [(ILIndex expr fieldexpr)
       (match-define `(,_ ,e-free) (find expr defs))
       (match-define `(,_ ,f-free) (find fieldexpr defs))
       (list (set) (set-union e-free f-free))]
      [(ILValue v) (list (set) (set))]
      [(ILUndefined) (list (set) (set))]
      [(ILNull) (list (set) (set))]
      [(ILNew e) (find e defs)]
      [(? symbol? v)
       (list (set)
             (if (set-member? defs v)
                 (set)
                 (set v)))]))

  (let loop ([defs defs]
             [free (set)]
             [stms stms*])
    (match stms
      [`() (list defs free)]
      [`(,hd . ,tl)
       (match-define `(,h-defs ,h-free) (find hd defs))
       (loop (set-union h-defs defs)
             (set-union h-free free)
             tl)])))

(define (free-identifiers stms)
  (second (free+defined stms (set))))

(module+ test
  (check-equal? (free-identifiers (list
                                   (ILVarDec 'a (ILValue 0))
                                   (ILReturn 'a)))
                (set))

  (check-equal? (free-identifiers (list
                                   (ILAssign 'a (ILValue 0))
                                   (ILReturn 'a)))
                (set 'a))

  (check-equal? (free-identifiers (list
                                   (ILAssign 'a (ILValue 0))
                                   (ILVarDec 'b (ILValue 0))
                                   (ILReturn 'c)))
                (set 'a 'c))

  (check-equal? (free-identifiers (list
                                   (ILLambda '(a b)
                                             (list (ILApp '+ '(a b))))))
                (set '+))

  (check-equal? (free-identifiers (list
                                   (ILLambda '(a b)
                                             (list (ILApp '+ '(a b c))))))
                (set 'c '+))

  (check-equal? (free-identifiers
                 (list
                  (ILLambda '(fn a)
                            (list
                             (ILIf*
                              (list
                               (ILIfClause (ILApp 'fn '())
                                           '(a))
                               (ILIfClause (ILApp 'foo '(a b))
                                           (list
                                            (ILAssign 'bar 'foo)
                                            'c))))))))
                (set 'foo 'b 'c 'bar)))

;;-----------------------------------------------------------------------------

;; Returns true if any subexpression of given input contains an
;; application.
;; TODO added suffixes to underscore patterns so that I don't have to handle them for real,
;;      but might want to at some point
(define has-application?
  (struct-match-lambda
    [(ILLambda _fst _snd) #f]
    [(ILBinaryOp _fst args) (ormap has-application? args)]
    [(ILApp _fst _snd) #t]
    [(ILArray items) (ormap has-application? items)]
    [(ILObject items) (ormap (λ (pair)
                               (has-application? (cdr pair)))
                             items)]
    [(ILRef expr _fst) (has-application? expr)]
    [(ILIndex expr fieldexpr) (or (has-application? expr)
                                  (has-application? fieldexpr))]
    [(ILValue _fst) #f]
    [(ILUndefined) #f]
    [(ILNull) #f]
    [(ILNew _fst) #t]
    [(ILInstanceOf expr type) (or (has-application? expr)
                                  (has-application? type))]
    [(ILTypeOf expr) (has-application? expr)]
    [(? symbol? e) #f]))
(module+ test
  (check-false (has-application? (ILValue 10)))
  (check-false (has-application? (ILBinaryOp '+ (list (ILValue 10) (ILValue 12)))))
  (check-false (has-application? (ILLambda '() (list (ILApp 'foo '())))))
  (check-true (has-application? (ILBinaryOp '+ (list (ILApp 'add1 (list (ILValue 10)))
                                                     (ILValue 10))))))

;;-----------------------------------------------------------------------------
;; Compute used and defined sets

(define (used+defined/block stms lam-block?)
  (let loop ([let-decs (set)]
             [var-decs (set)]
             [defines  (set)]
             [used     (set)]
             [stms     stms])
    (match stms
      [`()
       ;; If lam-block? is true, the current block is top level block
       ;; of a lambda, and hence we remove all defines and uses of variables
       ;; declared in function scope.
       (define scope-defs (if lam-block?
                              (set-union let-decs var-decs)
                              let-decs))
       (list (set-subtract used scope-defs)
             (set-subtract defines scope-defs))]
      [`(,hd . ,tl)
       (match-define `(,hd-used ,hd-defs) (used+defined/statement hd))
       (define final-defines (set-union defines hd-defs))
       (define final-used    (set-union used hd-used))
       (struct-match hd
         [(ILVarDec id _) (loop let-decs
                                (set-add var-decs id)
                                final-defines
                                final-used
                                tl)]
         [(ILLetDec id _) (loop (set-add let-decs id)
                                var-decs
                                final-defines
                                final-used
                                tl)]
         [_ (loop let-decs
                  var-decs
                  final-defines
                  final-used
                  tl)])])))

(define used+defined/statement
  (struct-match-lambda
    [(ILVarDec id expr)
     (match-define `(,used ,_) (if expr
                                     (used+defined/statement expr)
                                     (list (set) (set))))
     (list used (set id))]
    [(ILLetDec id expr)
     (match-define `(,used ,_) (if expr
                                     (used+defined/statement expr)
                                     (list (set) (set))))
     (list used (set id))]
    [(ILIf pred t-branch f-branch)
     (match-define `(,p-used ,_) (used+defined/statement pred))
     (match-define `(,t-used ,t-defs) (used+defined/block t-branch #f))
     (match-define `(,f-used ,f-defs) (used+defined/block f-branch #f))
     (list (set-union p-used t-used f-used)
           (set-union t-defs f-defs))]
    [(ILAssign lvalue rvalue)
     (match-define `(,lv-used ,_) (used+defined/statement lvalue))
     (match-define `(,rv-used ,_) (used+defined/statement rvalue))
     (list rv-used lv-used)]
    [(ILWhile condition body)
     (match-define `(,cond-used ,_) (used+defined/statement condition))
     (match-define `(,body-used ,body-defs) (used+defined/block body #f))
     (list (set-union cond-used body-used) body-defs)]
    [(ILReturn expr) (used+defined/statement expr)]
    [(ILLambda args exprs)
     (match-define `(,e-used ,e-defs) (used+defined/block exprs #t))
     (define args-set (list->set (il-formals->list args)))
     (list (set-subtract e-used args-set) e-defs)]
    [(ILApp lam args)
     (match-define `(,l-used ,_) (used+defined/statement lam))
     (match-define `(,a-used ,_) (used+defined/block args #f))
     (list (set-union l-used a-used) (set))]
    [(ILBinaryOp oper args) (used+defined/block args #f)]
    [(ILArray items) (used+defined/block items #f)]
    [(and (ILObject items) stm)
     ;; Typically the fields of object are in context (this) of any
     ;; lambda, and hence should be overall be removed from used set.
     ;; However, since members of objects could be called with
     ;; different context (this) we can't determine this for sure, so
     ;; we consider fields as used overall if they are used in bodies.
     (define bodies (ILObject-bodies stm))
     (match-define `(,used ,defined) (used+defined/block bodies #f))
     (list used defined)]
    [(ILExnHandler try error catch finally)
     (match-define `(,t-used ,t-defs) (used+defined/block try #f))
     (match-define `(,c-used ,c-defs)
       (match-let ([(list u d) (used+defined/block catch #f)])
         (list (set-remove u error)
               (set-remove d error))))
     (match-define `(,f-used ,f-defs) (used+defined/block finally #f))
     (list (set-union t-used c-used f-used) (set-union t-defs c-defs f-defs))]
    [(ILRef expr fieldname)
     (used+defined/statement expr)]
    [(ILIndex expr fieldexpr)
     (match-define `(,e-used ,_) (used+defined/statement expr))
     (match-define `(,f-used ,_) (used+defined/statement fieldexpr))
     (list (set-union e-used f-used) (set))]
    [(ILValue v) (list (set) (set))]
    [(ILUndefined) (list (set) (set))]
    [(ILArguments) (list (set) (set))]
    [(ILThis) (list (set) (set))]
    [(ILNull) (list (set) (set))]
    [(ILNew e) (used+defined/statement e)]
    [(? symbol? v)
     (list (set v) (set))]))

(define (used+defined/statement* stms)
  (for/hash([stm stms])
    (values stm (used+defined/statement stm))))

(module+ test
  ;;  Trivial
  (check-equal? (used+defined/statement 'a) (list (set 'a) (set)))
  (check-equal? (used+defined/statement (ILAssign 'a 'b))
                (list (set 'b) (set 'a)))

  ;; Nested statements with blocks
  (check-equal? (used+defined/statement (ILIf (ILApp 'func '(a b))
                                              (list (ILReturn 'c))
                                              (list (ILReturn 'd))))
                (list (set 'func 'a 'b 'c 'd) (set)))
  (check-equal? (used+defined/statement (ILIf (ILApp 'func '(a b))
                                              (list (ILAssign 'c 'a))
                                              (list (ILAssign 'd 'b))))
                (list (set 'func 'a 'b) (set 'c 'd)))
  (check-equal? (used+defined/statement (ILIf (ILApp 'func '(a b))
                                              (list (ILVarDec 'c 'a))
                                              (list (ILVarDec 'd 'b))))
                (list (set 'func 'a 'b) (set 'c 'd))
                "VarDec's in `if` statement define binding at function scope.")
  (check-equal? (used+defined/statement (ILIf (ILApp 'func '(a b))
                                              (list (ILLetDec 'c 'a))
                                              (list (ILLetDec 'd 'b))))
                (list (set 'func 'a 'b) (set))
                "LetDec's in `if` statement define binding at local scope.")

  ;; Lambdas
  (check-equal? (used+defined/statement (ILLambda '(a)
                                                  (list
                                                   (ILApp 'func '(a c d)))))
                (list (set 'func 'c 'd) (set))
                "Identifiers c and d are free in lambda")
  (check-equal? (used+defined/statement (ILLambda '(a)
                                                  (list
                                                   (ILVarDec 'a 'b))))
                (list (set 'b) (set))
                "`a` is defined in lambda hence doesn't affect outer scope")
  (check-equal? (used+defined/statement (ILLambda '()
                                                  (list
                                                   (ILVarDec 'a 'b)
                                                   (ILApp '+ '(a a)))))
                (list (set '+ 'b) (set))
                "`a` is defined within scope but `b` is free")
  (check-equal? (used+defined/statement (ILLambda '()
                                                  (list
                                                   (ILAssign 'a 'b))))
                (list (set 'b) (set 'a))
                "`a` is free and defined hence in result")

  ;; Exception handler
  (check-equal? (used+defined/statement
                 (ILExnHandler (list (ILAssign 'a 'b))
                               'e
                               (list (ILApp 'log '(e a)))
                               '()))
                (list (set 'b 'log 'a) (set 'a)))

  ;; Objects
  (check-equal? (used+defined/statement
                 (ILObject
                  `{[foo . ,(ILLambda '(n)
                                    (list (ILReturn (ILApp 'bar '(n)))))]
                    [bar . ,(ILLambda '(n)
                                    (list (ILReturn (ILApp 'foo '(n)))))]}))
                (list (set 'foo 'bar) (set))
                "fields of object are typically in this, but context can be changed")
  (check-equal? (used+defined/statement
                 (ILObject
                  `{[foo . ,(ILLambda '(n)
                                      (list (ILAssign 'foo (ILApp 'bar '(n)))))]
                    [bar . ,(ILLambda '(n)
                                      (list (ILReturn (ILApp 'foo '(n)))))]}))
                (list (set 'foo 'bar) (set 'foo))
                "fields of objects part of this, but this can be changed"))

;;-----------------------------------------------------------------------------
;; Flatten if-else

(define flatten-if-else/expr
  (struct-match-lambda
    [(ILLambda args body) (ILLambda args (flatten-if-else/stm* body))]
    [(ILBinaryOp oper args) (ILBinaryOp oper (map flatten-if-else/expr args))]
    [(ILApp lam args) (ILApp (flatten-if-else/expr lam)
                             (map flatten-if-else/expr args))]
    [(ILArray items) (ILArray (map flatten-if-else/expr items))]
    [(ILObject items) (ILObject (map (λ ([item : ObjectPair])
                                       (cons (car item)
                                             (flatten-if-else/expr (cdr item))))
                                     items))]
    [(ILRef expr* fieldname) (ILRef (flatten-if-else/expr expr*) fieldname)]
    [(ILIndex expr* fieldexpr) (ILIndex (flatten-if-else/expr expr*)
                                        (flatten-if-else/expr fieldexpr))]
    [(ILNew expr*) (ILNew (flatten-if-else/expr expr*))]
    [(ILInstanceOf expr* type) (ILInstanceOf (flatten-if-else/expr expr*)
                                             (flatten-if-else/expr type))]
    [(ILTypeOf expr) (ILTypeOf (flatten-if-else/expr expr))]
    [v v]))

(define flatten-if-else/stm
  (struct-match-lambda
    [(ILIf pred t-branch (list (ILIf pred* t-branch* f-branch*)))
     (flatten-if-else/stm
      (ILIf* (list (ILIfClause pred t-branch)
                   (ILIfClause pred* t-branch*)
                   (ILIfClause #f f-branch*))))]
    [(ILIf pred t-branch (list (ILIf* clauses)))
     (flatten-if-else/stm
      (ILIf* (cons (ILIfClause pred t-branch) clauses)))]
    [(ILIf pred t-branch f-branch) (ILIf (flatten-if-else/expr pred)
                                         (flatten-if-else/stm* t-branch)
                                         (flatten-if-else/stm* f-branch))]
    [(ILIf* (list clauses ... (ILIfClause #f (list (ILIf pred* t-branch* f-branch*)))))
     (flatten-if-else/stm
      (ILIf* (append clauses
                     (list
                      (ILIfClause pred* t-branch*)
                      (ILIfClause #f f-branch*)))))]
    [(ILIf* (list clauses ...))
     (ILIf* (map (λ ([c : IfClause])
                   (let* ([pred (ILIfClause-pred c)]
                          [body (ILIfClause-body c)]
                          [pred* (and pred (flatten-if-else/expr pred))]
                          [body* (flatten-if-else/stm* body)])
                     (ILIfClause pred* body*)))
                 clauses))]

    ;; Traverse through statements

    [(ILVarDec id expr) (ILVarDec id (and expr (flatten-if-else/expr expr)))]
    [(ILLetDec id expr) (ILLetDec id (and expr (flatten-if-else/expr expr)))]
    [(ILAssign id expr) (ILAssign id (flatten-if-else/expr expr))]
    [(ILWhile pred body) (ILWhile (flatten-if-else/expr pred) (flatten-if-else/stm* body))]
    [(ILReturn expr) (ILReturn (flatten-if-else/expr expr))]
    [(ILExnHandler try error catch finally) (ILExnHandler (flatten-if-else/stm* try)
                                                          error
                                                          (flatten-if-else/stm* catch)
                                                          (flatten-if-else/stm* finally))]
    [(ILThrow expr) (ILThrow (flatten-if-else/expr expr))]
    [(? ILLabel? stm) stm]
    [(? ILContinue? stm) stm]
    [(? ILExpr? stm) (flatten-if-else/expr stm)]))

(define (flatten-if-else/stm* stms)
  (map flatten-if-else/stm stms))

(define (flatten-if-else stms)
  (converge flatten-if-else/stm* stms))

(module+ test
  (check-equal? (flatten-if-else
                 (list (ILIf (ILValue 'pred1)
                             (list 't-branch-1)
                             (list
                              (ILIf (ILValue 'pred2)
                                    (list 't-branch-2)
                                    (list (ILIf (ILValue 'pred3)
                                                (list 't-branch-3)
                                                (list 'done))))))))
                (list (ILIf* (list
                              (ILIfClause (ILValue 'pred1) (list 't-branch-1))
                              (ILIfClause (ILValue 'pred2) (list 't-branch-2))
                              (ILIfClause (ILValue 'pred3) (list 't-branch-3))
                              (ILIfClause #f (list 'done)))))))
