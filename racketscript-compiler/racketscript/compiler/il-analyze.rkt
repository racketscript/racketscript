#lang typed/racket/base

(require racket/match
         racket/list
         racket/set
         "language.rkt"
         "util.rkt"
         "il.rkt")

(provide self-tail->loop
         free-identifiers
         free+defined)

;; TODO: Remove dead return statements.
;; TODO: Variable number arguments.
;; TODO: Identify self calls which are not simple symbol names.
;;       Eg. ILRef
;; TODO: Identify mutually recursive calls.

(define-type ILLink    (Option ILStatement))
(define-type ILResult  (U ILStatement* ILStatement))
(define-type IdentSet  (Setof Symbol))

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

  (: removed-declarations (Parameter (Setof Symbol)))
  (define removed-declarations (make-parameter ((inst set Symbol))))

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
       (ILObject (map (λ ([i : (Pairof ObjectKey ILExpr)])
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
       (match (next-statement)
         [(ILReturn e) #:when (and expr (equal? e id))
          (removed-declarations (set-add (removed-declarations) id))
          (ILReturn (handle-expr expr))]
         [_ (add-to-scope! id)
            (ILVarDec id (and expr
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
       (match-define (list t-branch* t-removed)
         (parameterize ([removed-declarations (removed-declarations)])
           (define s* (handle-stm* t-branch))
           (list s* (removed-declarations))))
       (match-define (list f-branch* f-removed)
         (parameterize ([removed-declarations (removed-declarations)])
           (define s* (handle-stm* f-branch))
           (list s* (removed-declarations))))
       (removed-declarations (set-intersect t-removed f-removed))
       (ILIf (handle-expr pred) t-branch* f-branch*)]
      [(ILWhile condition body)
       (ILWhile (handle-expr condition)
                (handle-stm* body))]
      [(ILReturn expr)
       (if (and (symbol? expr)
                (set-member? (removed-declarations) expr))
           '()
           (ILReturn (handle-expr expr)))]
      [(? ILExpr? expr) (handle-expr expr)]))

  (: handle-stm* (-> ILStatement* ILStatement*))
  (define (handle-stm* stm*)
    (let loop ([result : ILStatement* '()]
               [stm* stm*])
      (match stm*
        ['() result]
        [(cons hd tl)
         (define next : ILLink (if (empty? tl)
                                   (next-statement)
                                   (car tl)))
         (define stm** : ILResult
           (if (empty? tl)
               (handle-stm hd)
               (parameterize ([next-statement (car tl)])
                 (handle-stm hd))))

         (match stm**
           [(? ILStatement? stm)
            (loop (append result (list stm)) tl)]
           [(? ILStatement*? stms)
            (loop (append result stms) tl)])])))

  ;; Try lifting until convergence
  (let ([result (handle-stm* il*)])
    (if (equal? result il*)
        result
        (lift-returns result))))


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
                         (map (λ ([item : (Pairof ObjectKey ILExpr)])
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
         [else (ILReturn (ILApp lam (map handle-expr/general args)))])]
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


(: free+defined (-> ILStatement* IdentSet (List IdentSet IdentSet)))
(define (free+defined stms* defs)
  (define find* free+defined)

  (: find (-> ILStatement IdentSet (List IdentSet IdentSet)))
  ;; Returns (list defined-idents free-idents)
  (define (find stm defs)
    (match stm
      [(ILVarDec id expr)
       (list (set id)
             (if expr
                 (second (find expr (set-add defs id)))
                 (set)))]
      [(ILIf pred t-branch f-branch)
       (match-define (list _ p-free) (find pred defs))
       (match-define (list t-defs t-free) (find* t-branch defs))
       (match-define (list f-defs f-free) (find* f-branch defs))
       (list (set-union t-defs f-defs)
             (set-union p-free t-free f-free))]
      [(ILAssign lvalue rvalue)
       (match-define (list _ lv-free) (find lvalue defs))
       (match-define (list _ rv-free) (find rvalue defs))
       (list (set) (set-union lv-free rv-free))]
      [(ILWhile condition body)
       (match-define (list _ cond-free) (find condition defs))
       (match-define (list body-defs body-free) (find* body defs))
       (list body-defs
             (set-union cond-free body-free))]
      [(ILReturn expr) (find expr defs)]
      [(ILLambda args expr)
       (match-define (list _ e-free)
         (find* expr (set-union defs (list->set args))))
       (list (set) e-free)]
      [(ILApp lam args)
       (match-define (list _ l-free) (find lam defs))
       (match-define (list _ a-free) (find* args defs))
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
       (match-define (list _ e-free) (find expr defs))
       (match-define (list _ f-free) (find fieldexpr defs))
       (list (set) (set-union e-free f-free))]
      [(ILValue v) (list (set) (set))]
      [(ILNew e) (find e defs)]
      [(? symbol? v)
       (list (set)
             (if (set-member? defs v)
                 (set)
                 (set v)))]))

  (let loop ([defs : IdentSet defs]
             [free : IdentSet (set)]
             [stms stms*])
    (match stms
      ['() (list defs free)]
      [(cons hd tl)
       (match-define (list h-defs h-free) (find hd defs))
       (loop (set-union h-defs defs)
             (set-union h-free free)
             tl)])))

(: free-identifiers (-> ILStatement* IdentSet))
(define (free-identifiers stms)
  (second (free+defined stms (set))))

(module+ test
  (require typed/rackunit)

  (define-syntax-rule (check-equal?* args ...)
    (parameterize ([fresh-id-counter 0])
      (check-equal? args ...)))

  ;; Self tail to loops -------------------------------------------------------

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
      '(n a)
      (list
       (ILLabel 'lambda-start1)
       (ILWhile
        (ILValue #t)
        (list
         (ILIf (ILApp 'zero? '(n))
               (list (ILReturn 'a))
               (list
                (ILVarDec 'n2 (ILApp 'sub1 '(n)))
                (ILVarDec 'a3 (ILBinaryOp '* '(n a)))
                (ILAssign 'n 'n2)
                (ILAssign 'a 'a3)
                (ILContinue 'lambda-start1)))))))))
   "Translate self tail recursive factorial to loops")

  ;; Check return lifting -----------------------------------------------------

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

  ;; Test free-identifer ------------------------------------------------------

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
                (set 'c '+)))
