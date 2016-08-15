#lang typed/racket/base

;;; Generate IL code from abstract syntax. Each binding name
;;; in assumed to be fresh, to enforce lexical scope rules of Racket

(require racket/match
         racket/function
         racket/list
         racket/format
         racket/path
         racket/set
         "config.rkt"
         "global.rkt"
         "util.rkt"
         "environment.rkt"
         "absyn.rkt"
         "il.rkt")

(require/typed "global.rkt"
  [global-unreachable-idents (HashTable Path (Setof Symbol))])

(provide absyn-top-level->il
         absyn-gtl-form->il
         absyn-expr->il
         absyn-module->il)

(define-type-alias ModuleObjectNameMap (HashTable (U Path Symbol) Symbol))
(: module-object-name-map (Parameter ModuleObjectNameMap))
(define module-object-name-map (make-parameter ((inst hash (U Path Symbol) Symbol))))

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
  
  (match-define (Module id path lang imports forms) mod)
  (printf "[il] ~a\n" id)

  (define imported-mod-path-list (set->list imports))

  (: requires* (Listof ILRequire))
  (define requires*
    (for/list ([mod-path imported-mod-path-list]
               [counter (in-naturals)])
      (define mod-obj-name (string->symbol (~a "M" counter))) ;; FIXME: Make a unique name in module
      (define import-name
        (match mod-path
          [(? symbol? _) (jsruntime-import-path path (jsruntime-module-path mod-path))]
          [_ (module->relative-import (cast mod-path Path))]))
      (ILRequire import-name mod-obj-name)))

  (define mod-stms
    (parameterize ([module-object-name-map ;; Names we will use for module objects in JS
                    (for/fold ([acc (module-object-name-map)])
                              ([req requires*]
                               [mod-path imported-mod-path-list])
                      (hash-set acc
                                mod-path
                                (ILRequire-name req)))])
      (append-map
       (λ ([form : ModuleLevelForm])
         (cond
           [(GeneralTopLevelForm? form) (absyn-gtl-form->il form)]
           [(Provide*? form) (add-provides! (absyn-provide->il form)) '()]
           [(SubModuleForm? form) '()])) ;; TODO
       forms)))

  ;; Due to macro expansion we may have identifiers which are not actually
  ;; exported by that particular module. We find such identifiers here
  ;; and put them in provide list
  (: unreachable-ident-provides (Listof ILProvide))
  (define unreachable-ident-provides
    (let ([ident-set ((inst hash-ref! Path (Setof Symbol))
                      global-unreachable-idents
                      path
                      set)])
      (for/list ([ident (in-set ident-set)])
        (ILProvide ident))))

  (ILModule path
            (append (unbox provides) unreachable-ident-provides)
            requires*
            mod-stms))

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
    [(PlainLambda formals body)
     ;; TODO: This is terribly mixed up lower level details. Perhaps something with
     ;;       IL can be improved this avoid this madness?
     (: ->jslist (-> ILExpr ILExpr))
     (define (->jslist f)
       (ILApp (name-in-module 'core 'Pair.listFromArray) (list f)))
     (define arguments-array (ILApp (name-in-module 'core 'argumentsToArray)
                                    (list 'arguments)))
     (define-values (il-formals stms-formals-init)
       (cond
         [(symbol? formals) (values '()
                                    (list
                                     (ILVarDec formals (->jslist arguments-array))))]
         [(list? formals) (values formals '())]
         [(cons? formals)
          (define fi (car formals))
          (define fp (cdr formals))
          (define fi-len (length fi))
          (values
           '()
           (append1
            (map (λ ([i : Natural] [f : Symbol])
                   (ILVarDec f (ILIndex 'arguments (ILValue i))))
                 (range fi-len)
                 fi)
            (ILVarDec fp
                      (->jslist (ILApp (name-in-module 'core 'sliceArguments)
                                       (list arguments-array (ILValue fi-len)))))))]))
     (define-values (body-stms body-value)
       (for/fold/last ([stms : ILStatement* '()]
                       [rv : ILExpr (ILValue (void))])
                      ([e last? body])
                      (define-values (s v) (absyn-expr->il e))
                      (if last?
                          (values (append stms s) v)
                          (values (append stms s (list v)) v))))
     (values '()
             (ILLambda il-formals
                       (append stms-formals-init
                               body-stms
                               (list (ILReturn body-value)))))]
    [(CaseLambda clauses)
     (absyn-expr->il (expand-case-lambda expr))]
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
    [(PlainApp '#%js-ffi args)
     (match args
       [(list (Quote 'var) (Quote var)) (values '() (cast var Symbol))]
       [(list (Quote 'ref) b xs ...)
        (define-values (stms il) (absyn-expr->il b))
        (values stms
                (for/fold ([il il])
                          ([x xs])
                  (match-define (Quote s) x) ;; Previous phase wrap the symbol in quote
                  ;; HACK: To avoid wrapping the base symbol into Symbol class
                  ;; We have to generate code context sensitively
                  (if (ILValue? il)
                      (ILRef (cast (ILValue-v il) Symbol)
                             (cast s Symbol))
                      (ILRef il (cast s Symbol)))))]
       [(list (Quote 'index) b xs ...)
        (define-values (stms il) (absyn-expr->il b))
        (for/fold ([stms stms]
                   [il il])
                  ([x xs])
          (define-values (x-stms s-il) (absyn-expr->il x))
          (values (append stms x-stms)
                  (if (ILValue? il)
                      (ILIndex (cast (ILValue-v il) Symbol)
                               s-il)
                      (ILIndex il s-il))))]
       [(list (Quote 'app) lam args ...)
        (define-values (stms il) (absyn-expr->il lam))
        (define-values (stms* args*)
          (for/fold ([stms : ILStatement* stms]
                     [args-val : (Listof ILExpr) '()])
                    ([a args])
            (define-values (a-stms a-il) (absyn-expr->il a))
            (values (append stms a-stms)
                    (append args-val (list a-il)))))
        (values stms*
                (ILApp il args*))]
       [(list (Quote 'assign) lv rv)
        (define-values (lv-stms lv-il) (absyn-expr->il lv))
        (define-values (rv-stms rv-il) (absyn-expr->il rv))
        (values (append lv-stms
                        rv-stms
                        (list (ILAssign (cast lv-il ILLValue) rv-il)))
                (ILValue (void)))]
       [(list (Quote 'new) lv)
        (define-values (stms il) (absyn-expr->il lv))
        (values stms
                (ILNew (cast il ILLValue)))]
       [_ (displayln args) (error 'absyn-expr->il "unknown ffi form")])]
    [(PlainApp lam args)
     ;;NOTE: Comparision operators work only on two operands TODO later
     (: binops (Listof ImportedIdent))
     (define binops (map (λ ([b : Symbol]) (ImportedIdent b '#%kernel)) '(+ - * /)))

     (: il-app/binop (-> Ident (Listof ILExpr) (U ILApp ILBinaryOp)))
     (define (il-app/binop v arg*)
       (define v-il (let-values ([(_ v) (absyn-expr->il v)])
                      v))
       (cond
         [(and (equal? v (ImportedIdent '- '#%kernel)) (length=? arg* 1))
          (ILApp v-il arg*)]
         [(and (equal? v (ImportedIdent '/ '#%kernel)) (length=? arg* 1))
          (ILBinaryOp '/ (cons (ILValue 1) arg*))]
         [(and (ImportedIdent? v) (member v binops))
          (ILBinaryOp (ImportedIdent-id v) arg*)]
         [else (ILApp v-il arg*)]))

     (let loop ([arg-stms : ILStatement* '()] ;;; statements for computing arguments
                [arg* : (Listof ILExpr) '()] ;;; expressions passed to the lam
                [arg args])
       (match arg
         ['()
          (cond
            [(Ident? lam) (values arg-stms
                                   (il-app/binop lam arg*))]
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
    [(LocalIdent id) (values '() id)]
    [(TopLevelIdent id) (values '() id)]
    [(ImportedIdent id src)
     ;; find the name of object of imported js module and make a ref
     ;; to access this
     (define mod-obj-name (hash-ref (module-object-name-map) src))
     (values '() (ILRef (assert mod-obj-name symbol?) id))]
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
     (: binding-stms ILStatement*)
     (define binding-stms
       (for/list ([i : Natural (range (length args))]
                  [arg : Symbol args])
         (ILValuesMatch arg result-id i)))
     (append stms
             (cons (ILVarDec result-id v)
                   binding-stms))]))

(: absyn-value->il (-> Any ILValue))
(define (absyn-value->il d)
  (cond
    [(or (symbol? d)
         (string? d)
         (bytes? d)
         (integer? d)
         (list? d)
         (cons? d)
         (hash? d)
         (boolean? d)
         (vector? d)
         (struct? d)
         (keyword? d)
         (real? d))
     (ILValue d)]
    [else (error (~a "unsupported value" d))]))

(: expand-case-lambda (-> CaseLambda PlainLambda))
(define (expand-case-lambda clam)
  (define args (fresh-id 'args))
  (define apply-js-name (ImportedIdent 'apply '#%kernel))
  (: body Expr)
  (define body
    (let loop : Expr ([clauses* (CaseLambda-clauses clam)])
         (match clauses*
           ['() (PlainApp (ImportedIdent 'error '#%kernel)
                          (list (Quote "case-lambda: invalid case")))]
           [(cons hd tl)
            (If ((get-formals-predicate hd) (LocalIdent args))
                (PlainApp apply-js-name (list hd (LocalIdent args)))
                (loop tl))])))
  (PlainLambda args (list body)))
   
(: get-formals-predicate (-> PlainLambda (-> Expr Expr)))
(define (get-formals-predicate c)
  ;; TODO: Use binary ops instead of function call for cmp
  (define frmls (PlainLambda-formals c))
  (define length-js-name (ImportedIdent 'length '#%kernel))
  (cond
    [(symbol? frmls) (λ (_) (Quote #t))]
    [(list? frmls)
     (λ ([v : Expr])
       (PlainApp (ImportedIdent 'equal? '#%kernel)
                 (list 
                  (PlainApp length-js-name (list v))
                  (Quote (length frmls)))))]
    [(cons? frmls)
     (λ ([v : Expr])
       (PlainApp (ImportedIdent '>= '#%kernel)
                 (list
                  (PlainApp length-js-name (list v))
                  (Quote (sub1 (length (improper->proper frmls)))))))]))


(module+ test
  (require typed/rackunit
           racket/pretty)
 
  (define-syntax-rule (values->list e)
    (call-with-values (λ () e) list))

  (define-syntax-rule (ilcheck fn form result ...)
    (parameterize ([fresh-id-counter 0]
                   [module-object-name-map
                    (hash '#%kernel 'kernel)])
      (check-equal?
       (values->list (fn form))
       (list result ...))))

  (define-syntax-rule (check-ilexpr expr stms val)
    (ilcheck absyn-expr->il expr stms val))

  (define-syntax-rule (check-iltoplevel form stms)
    (ilcheck absyn-top-level->il form stms))

  (define-syntax-rule (check-ilgtl form stms)
    (ilcheck absyn-gtl-form->il form stms))

  (define LI LocalIdent)
  (define LI* (λ s* ((inst map LocalIdent Symbol) LocalIdent (cast s* (Listof Symbol)))))

  (: kident (-> Symbol ImportedIdent))
  (define (kident s) (ImportedIdent s '#%kernel))

  (define-syntax-rule (convert+print fn absyn)
    (parameterize ([fresh-id-counter 0]
                   [module-object-name-map
                    (hash '#%kernel 'kernel)])
      (pretty-print
       (values->list (fn absyn)))))
  
  ;; enable test environment
  (test-environment? #t)

  ;; Expressions --------------------------------------------------------------

  ;; Values

  (check-ilexpr (Quote 42)        '() (ILValue 42))
  (check-ilexpr (Quote "Hello")   '() (ILValue "Hello"))
  (check-ilexpr (Quote 'hello)    '() (ILValue 'hello))
  (check-ilexpr (Quote '(1 2 3))  '() (ILValue '(1 2 3)))
  (check-ilexpr (Quote '(1 (2 3) 4 (a b))) '() (ILValue '(1 (2 3) 4 (a b))))
  (check-ilexpr (Quote '(1 . 2))  '() (ILValue '(1 . 2)))
  (check-ilexpr (Quote #f)        '() (ILValue #f))

  ;; Function application

  (check-ilexpr (PlainLambda '(x) (LI* 'x))
                '()
                (ILLambda '(x) (list (ILReturn 'x))))
  (check-ilexpr (PlainLambda 'x (LI* 'x))
                '()
                (ILLambda
                 '()
                 (list
                  (ILVarDec
                   'x
                   (ILApp
                    (name-in-module 'core 'Pair.listFromArray)
                    (list (ILApp (name-in-module 'core 'argumentsToArray)
                                 '(arguments)))))
                  (ILReturn 'x))))
  ;; If expressions

  (check-ilexpr (If (Quote #t) (Quote 'yes) (Quote 'no))
                (list
                 (ILIf
                  (ILValue #t)
                  (list (ILVarDec 'if_res1 (ILValue 'yes)))
                  (list (ILVarDec 'if_res1 (ILValue 'no)))))
                'if_res1)

  ;; Lambdas

  (check-ilexpr (PlainLambda '(x) (LI* 'x))
                '()
                (ILLambda
                 '(x)
                 (list (ILReturn 'x))))
  (check-ilexpr (PlainLambda '(a b)
                             (list (PlainApp (LocalIdent 'list) (LI* 'a 'b))))
                '()
                (ILLambda '(a b)
                          (list (ILReturn (ILApp 'list '(a b))))))
  
  ;; Let expressions
  (check-ilexpr (LetValues (list (cons '(a) (Quote 1))
                                 (cons '(b) (Quote 2)))
                           (LI* 'a 'b))
                (list (ILVarDec 'a (ILValue 1)) (ILVarDec 'b (ILValue 2))
                      'a)
                'b)
  (check-ilexpr (LetValues (list (cons '(a)
                                       (If (Quote #t)
                                           (Quote 'yes)
                                           (Quote 'false)))
                                 (cons '(b)
                                       (PlainApp (kident '+)
                                                 (list
                                                  (Quote 1) (Quote 2)))))
                           (list (PlainApp (LocalIdent 'list) (LI* 'a 'b))))
                (list
                 (ILIf
                  (ILValue #t)
                  (list (ILVarDec 'if_res1 (ILValue 'yes)))
                  (list (ILVarDec 'if_res1 (ILValue 'false))))
                 (ILVarDec 'a 'if_res1)
                 (ILVarDec 'b (ILBinaryOp '+
                                          (list
                                           (ILValue 1) (ILValue 2)))))
                (ILApp 'list '(a b)))

  ;; Binary operations

  (check-ilexpr (PlainApp (kident '+) (list (Quote 1) (Quote 2)))
                '()
                (ILBinaryOp '+ (list (ILValue 1) (ILValue 2))))
  (check-ilexpr (PlainApp (kident '-) (list (Quote 1) (Quote 2) (Quote 3)))
                '()
                (ILBinaryOp '-
                            (list
                             (ILValue 1) (ILValue 2) (ILValue 3))))

  ;; Case Lambda

  (check-ilexpr
   (CaseLambda
    (list
     (PlainLambda '(a b) (list (PlainApp (kident 'add) (LI* 'a 'b))))
     (PlainLambda '(a b c) (list (PlainApp (kident 'mul) (LI* 'a 'b 'c))))))
   '()
   (ILLambda
    '()
    (list
     (ILVarDec
      'args1
      (ILApp
       (name-in-module 'core 'Pair.listFromArray)
       (list (ILApp (name-in-module 'core 'argumentsToArray)
                    '(arguments)))))
     (ILIf
      (ILApp
       (ILRef 'kernel 'equal?)
       (list (ILApp (ILRef 'kernel 'length) '(args1))
             (ILValue 2)))
      (list
       (ILVarDec
        'if_res3
        (ILApp
         (ILRef 'kernel 'apply)
         (list
          (ILLambda '(a b)
                    (list (ILReturn (ILApp (ILRef 'kernel 'add) '(a b)))))
          'args1))))
      (list
       (ILIf
        (ILApp
         (ILRef 'kernel 'equal?)
         (list (ILApp (ILRef 'kernel 'length) '(args1))
               (ILValue 3)))
        (list
         (ILVarDec
          'if_res2
          (ILApp
           (ILRef 'kernel 'apply)
           (list
            (ILLambda '(a b c)
                      (list
                       (ILReturn (ILApp (ILRef 'kernel 'mul) '(a b c)))))
            'args1))))
        (list
         (ILVarDec
          'if_res2
          (ILApp (ILRef 'kernel 'error)
                 (list (ILValue "case-lambda: invalid case"))))))
       (ILVarDec 'if_res3 'if_res2)))
     (ILReturn 'if_res3))))

  ;; FFI ----------------------------------------------------------------------

  ;; Top Level ----------------------------------------------------------------

  (check-iltoplevel
   (list (PlainApp (kident 'displayln) (list (Quote "hello")))
         (PlainApp (kident 'write) (list (Quote "what" ) (LocalIdent 'out))))
   (list
    (ILApp (ILRef 'kernel 'displayln) (list (ILValue "hello")))
    (ILApp (ILRef 'kernel 'write) (list (ILValue "what") 'out))))

  ;; General Top Level --------------------------------------------------------

  (check-ilgtl
   (DefineValues '(x) (Quote 42))
   (list (ILVarDec 'x (ILValue 42))))

  (check-ilgtl
   (DefineValues '(x ident)
     (PlainApp (kident 'values)
               (list (Quote 42)
                     (PlainLambda '(x) (LI* 'x)))))
   (list
    (ILVarDec
     'let_result1
     (ILApp (ILRef 'kernel 'values) (list (ILValue 42) (ILLambda '(x) (list (ILReturn 'x))))))
    (ILValuesMatch 'x 'let_result1 0)
    (ILValuesMatch 'ident 'let_result1 1))))
