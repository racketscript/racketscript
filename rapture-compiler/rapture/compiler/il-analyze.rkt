#lang typed/racket/base

(require racket/match
         racket/set
         "language.rkt"
         "util.rkt"
         "il.rkt")

(provide self-tail->loop)

;; TODO: Remove dead return statements.
;; TODO: Variable number arguments.
;; TODO: Identify self calls which are not simple symbol names.
;;       Eg. ILRef
;; TODO: Identify mutually recursive calls.

(define-type ILLink    (Option ILStatement))
(define-type ILResult  (U ILStatement* ILStatement))

(: self-tail->loop (-> ILStatement* ILStatement*))
;; Frontend function to convert self tail calls to
;; loops.
(define (self-tail->loop il*)
  (x-self-tail->loop (lift-returns il*)))

(: lift-returns (-> ILStatement* ILStatement*))
;; Lifts return statements up by one statement, if
;; the identifier returned, was declared or re-assigned
;; in previous statement.
(define (lift-returns il*)
  (: current-scope-declarations (Parameter (Setof Symbol)))
  ;; Track all variables delcared in current function scope.
  (define current-scope-declarations (make-parameter ((inst set Symbol))))

  (: next-statement (Parameter ILLink))
  ;; Next statement that will be executed after
  ;; the statement we are currently handling.
  (define next-statement (make-parameter #f))

  (: add-to-scope! (-> Symbol Void))
  ;; Adds sym to scope of declared variables.
  (define (add-to-scope! sym)
    (current-scope-declarations
     (set-add (current-scope-declarations) sym)))

  ;; Returns true if we have discovered `sym`
  (define (var-in-scope? sym)
    (set-member? (current-scope-declarations) sym))

  (: handle-expr (-> ILExpr ILExpr))
  (define (handle-expr e)
    (match e
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
       (ILObject (map (λ ([i : (Pairof Symbol ILExpr)])
                        (cons (car i)
                              (handle-expr (cdr i))))
                      items))]
      [(ILRef expr fieldname)
       (ILRef (handle-expr expr) fieldname)]
      [(ILIndex expr fieldexpr)
       (ILIndex (handle-expr expr) (handle-expr fieldexpr))]
      [(ILNew v)
       (ILNew (cast (handle-expr v) (U Symbol ILRef ILIndex ILApp)))]
      [(ILValue v) e]
      [(? symbol? v) v]))

  (: handle-stm (-> ILStatement ILResult))
  (define (handle-stm stm)
    (match stm
      [(ILVarDec id expr)
       (add-to-scope! id)
       (match (next-statement)
         [(ILReturn e) #:when (and expr
                                   (var-in-scope? id)
                                   (equal? e id))
          (ILReturn (handle-expr expr))]
         [_ (ILVarDec id (and expr
                              (handle-expr expr)))])]
      [(ILAssign lvalue rvalue)
       (match (next-statement)
         [(ILReturn e) #:when (and (symbol? lvalue)
                                   (var-in-scope? lvalue)
                                   (equal? e lvalue))
          (ILReturn (handle-expr rvalue))]
         [_ (ILAssign (cast (handle-expr lvalue) ILLValue)
                      (handle-expr rvalue))])]
      [(ILIf pred t-branch f-branch)
       (ILIf (handle-expr pred)
             (handle-stm* t-branch)
             (handle-stm* f-branch))]
      [(ILWhile condition body)
       (ILWhile (handle-expr condition)
                (handle-stm* body))]
      [(ILReturn expr) (ILReturn (handle-expr expr))]
      [(? ILExpr? expr) (handle-expr expr)]))

  (: handle-stm* (-> ILStatement* ILStatement*))
  (define (handle-stm* stm*)
    (let loop ([result : ILStatement* '()]
               [stm* stm*])
      (match stm*
        ['() result]
        [(cons hd tl)
         (define next : ILLink (if (null? tl)
                                   (next-statement)
                                   (car tl)))
         (define stm** : ILResult
           (if (null? tl)
               (handle-stm hd)
               (parameterize ([next-statement (car tl)])
                 (handle-stm hd))))

         (match stm**
           [(? ILStatement? stm)
            (loop (append result (list stm)) tl)]
           [(? ILStatement*? stms)
            (loop (append result stms) tl)])])))

  (let loop ([result : ILStatement* '()])
    ;; Try lifting until convergence
    (let ([result* (parameterize ([current-scope-declarations (set)]
                                  [next-statement #f])
                     (handle-stm* il*))])
      (if (equal? result result*)
          result
          (loop result*)))))


(: x-self-tail->loop (-> ILStatement* ILStatement*))
;; Translate self tail calls to loop. The given `il`
;; is assumed to have gone through return lifting.
(define (x-self-tail->loop il)
  (: wrap-with-while? (Parameter Boolean))
  (define wrap-with-while? (make-parameter #f))

  (: lambda-name (Parameter (Option Symbol)))
  (define lambda-name (make-parameter #f))

  (: lambda-formals (Parameter (Listof Symbol)))
  (define lambda-formals (make-parameter '()))

  (: lambda-start-label (Parameter (Option Symbol)))
  (define lambda-start-label (make-parameter #f))

  (: handle-expr/general (-> ILExpr ILExpr))
  (define (handle-expr/general e) (handle-expr e #f #f))

  (: handle-expr (-> ILExpr Boolean (Option Symbol) ILExpr))
  (define (handle-expr e tail-position? vardec)
    (match e
      [(ILLambda args body)
       (parameterize ([lambda-name vardec]
                      [lambda-formals args]
                      [lambda-start-label (fresh-id 'lambda-start)]
                      [wrap-with-while? #f])
         (define body* : ILStatement*
           (let ([s* (handle-stm* body)])
             (if (wrap-with-while?)
                 (list (ILLabel (cast (lambda-start-label) Symbol))
                       (ILWhile (ILValue #t) s*))
                 s*)))
         (ILLambda args body*))]
      [(ILApp lam args)
       (ILApp lam (map handle-expr/general args))]
      [(ILBinaryOp oper args)
       (ILBinaryOp oper (map handle-expr/general args))]
      [(ILArray items) (ILArray (map handle-expr/general items))]
      [(ILObject items) (ILObject
                         (map (λ ([item : (Pairof Symbol ILExpr)])
                                (cons (car item)
                                      (handle-expr/general (cdr item))))
                              items))]
      [(ILRef expr fieldname) (ILRef (handle-expr/general expr) fieldname)]
      [(ILIndex expr fieldname) (ILIndex (handle-expr/general expr)
                                         (handle-expr/general fieldname))]
      [(ILValue v) e]
      [(ILNew v) e]
      [(? symbol? v) e]))

  (: handle-stm (-> ILStatement ILResult))
  (define (handle-stm s)
    (match s
      [(ILReturn (ILApp lam args))
       (cond
         [(and (equal? (lambda-name) lam)
               (equal? (length (lambda-formals))
                       (length args)))
          ;; Its self recursive call
          (wrap-with-while? #t)
          (define compute-args : (Listof ILVarDec)
            (for/list  ([f (lambda-formals)]
                        [a args])
              (ILVarDec (fresh-id f) (handle-expr/general a))))
          (define reset-args : (Listof ILAssign)
            (for/list ([f : Symbol (lambda-formals)]
                       [ca compute-args])
              (ILAssign f (ILVarDec-id ca) )))
          (append compute-args
                  reset-args
                  (list (ILContinue
                         (cast (lambda-start-label) Symbol))))]
         [else (ILApp lam (map handle-expr/general args))])]
      [(ILVarDec id expr)
       (ILVarDec id (and expr
                         (handle-expr expr #f id)))]
      [(ILIf pred t-branch f-branch)
       (ILIf (handle-expr/general pred)
             (handle-stm* t-branch)
             (handle-stm* f-branch))]
      [(ILAssign lvalue rvalue)
       ;; TODO: Handle ILLValue
       (if (symbol? lvalue)
           (ILAssign lvalue (handle-expr rvalue #f lvalue))
           (ILAssign lvalue (handle-expr rvalue #f #f)))]
      [(ILWhile condition body)
       (ILWhile (handle-expr/general condition)
                (handle-stm* body))]
      [(ILReturn expr) (ILReturn (handle-expr expr #t #f))]
      [(ILContinue n) s]
      [(ILLabel n) s]
      [(? ILExpr? expr) (handle-expr/general expr)]))

  (: handle-stm* (-> ILStatement* ILStatement*))
  (define (handle-stm* s*)
    (for/fold ([result : ILStatement* '()])
              ([s s*])
      (define stm* (handle-stm s))
      (match stm*
        [(? ILStatement? r) (append result (list r))]
        [(? ILStatement*? r*) (append result r*)])))

  (handle-stm* il))

(module+ test
  (require typed/rackunit)

  (define-syntax-rule (check-equal?* args ...)
    (parameterize ([fresh-id-counter 0])
      (check-equal? args ...)))

  (check-equal?*
   (x-self-tail->loop
    (list
     (ILVarDec 'fact
               (ILLambda '(n a)
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
      '(n a)
      (list
       (ILLabel 'lambda-start1)
       (ILWhile
        (ILValue #t)
        (list
         (ILIf
          (ILApp 'zero? '(n))
          (list (ILReturn 'a))
          (list
           (ILVarDec 'n2 (ILApp 'sub1 '(n)))
           (ILVarDec 'a3 (ILBinaryOp '* '(n a)))
           (ILAssign 'n 'n2)
           (ILAssign 'a 'a3)
           (ILContinue 'lambda-start1)))))))))
   "Translate self tail recursive calls to loops")

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
          (ILApp 'fact (list (ILApp 'sub1 '(n2)) (ILBinaryOp '* '(n2 a3)))))))
       (ILReturn 'if_res2)))))
   "Lift return statements up.")

  (require racket/pretty)

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
   "Lift last return statement."))
