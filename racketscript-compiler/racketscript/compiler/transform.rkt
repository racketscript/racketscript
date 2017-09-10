#lang typed/racket/base

;;; Generate IL code from abstract syntax. Each binding name
;;; in assumed to be fresh, to enforce lexical scope rules of Racket

(require racket/match
         racket/function
         racket/bool
         racket/list
         racket/format
         racket/path
         racket/set
         racket/syntax
         typed/racket/unsafe
         threading
         anaphoric
         "config.rkt"
         "global.rkt"
         "logging.rkt"
         "util.rkt"
         "environment.rkt"
         "absyn.rkt"
         "il.rkt"
         "il-analyze.rkt")

(require/typed racket/syntax
  [format-symbol (-> String Any * Symbol)])

(provide absyn-top-level->il
         absyn-gtl-form->il
         absyn-expr->il
         absyn-module->il)

(define-type-alias ModuleObjectNameMap (HashTable (U Path Symbol)
                                                  Symbol))

(: module-object-name-map (Parameter ModuleObjectNameMap))
(define module-object-name-map
  (make-parameter ((inst hash (U Path Symbol) Symbol))))


(: absyn-top-level->il (-> TopLevelForm ILStatement*))
(define (absyn-top-level->il form)
  (cond
    [(Module? form) (error 'absyn-top-level->il
                           "Not supported. Todo.")]
    [(Expr? form)
     (define-values (stmt v) (absyn-expr->il form #f))
     (append1 stmt v)]
    [(Begin? form) (absyn-expr->il form #f)]
    [else (error "only modules supported at top level")]))


(: absyn-module->il (-> Module ILModule))
(define (absyn-module->il mod)
  (match-define (Module id path lang imports quoted-bindings forms) mod)
  (log-rjs-info "[il] ~a" id)

  (: provides (Boxof ILProvide*))
  (define provides (box '()))

  ;; Since we get identifiers directly from defining module, we keep
  ;; track of defines, use this for exporting all defines or exclude
  ;; re-exports here
  (: top-level-defines (Setof Symbol))
  (define top-level-defines
    (list->set
     (append
      (append-map DefineValues-ids
                  (filter DefineValues? forms))
      (map JSRequire-alias
           (filter JSRequire? forms)))))

  (: add-provides! (-> ILProvide* Void))
  (define (add-provides! p*)
    (set-box! provides (append (unbox provides) p*)))

  (define imported-mod-path-list (set->list imports))

  (: requires* (Listof (Option ILRequire)))
  (define requires*
    ;; FIXME: We put #f so that we can match it exactly
    ;;  later in mod-stms
    (for/list ([mod-path imported-mod-path-list]
               [counter (in-naturals)])
      (define mod-obj-name (string->symbol (~a "M" counter)))
      (define import-name
        (match mod-path
          [(? symbol? _)
           (jsruntime-import-path path
                                  (jsruntime-module-path mod-path))]
          [_ (module->relative-import (cast mod-path Path))]))
      ;; See expansion of identifier in `expand.rkt` for primitive
      ;; modules
      (if (or (and (primitive-module? mod-path)  ;; a self-import cycle
                   (equal? path (actual-module-path mod-path)))
              (and (primitive-module-path? (actual-module-path path))
                   (set-member? ignored-module-imports-in-boot mod-path)))
          #f
          (ILRequire import-name mod-obj-name '*))))

  ;; Append all JavaScript requires we find at GTL over here
  (: js-requires (Boxof (Listof ILRequire)))
  (define js-requires (box '()))

  ;; Filter out #f requires*
  (: racket-requires ILRequire*)
  (define racket-requires (filter ILRequire? requires*))

  (define mod-stms
    (parameterize ([module-object-name-map
                    ; Names we will use for module objects in JS
                    (for/fold ([acc (module-object-name-map)])
                              ([req requires*]
                               [mod-path imported-mod-path-list])
                      (if (ILRequire? req)
                          (hash-set acc
                                    mod-path
                                    (ILRequire-name req))
                          acc))])
      (append-map
       (λ ([form : ModuleLevelForm])
         (cond
           [(JSRequire? form)
            (set-box! js-requires
                      (cons (ILRequire (~a (JSRequire-path form))
                                       (JSRequire-alias form)
                                       (JSRequire-mode form))
                            (unbox js-requires)))
            '()]
           [(GeneralTopLevelForm? form) (absyn-gtl-form->il form)]
           [(Provide*? form)
            (add-provides! (absyn-provide->il form top-level-defines)) '()]
           [(SubModuleForm? form) '()])) ;; TODO
       forms)))

  ;; Compute `provides` from this modoule

  (: final-provides ILProvide*)
  (define final-provides
    (let ([current-module-define?
           (λ (p)
             (match p
               [(ILSimpleProvide id)
                (set-member? top-level-defines id )]
               [(ILRenamedProvide local-id exported-id)
                (set-member? top-level-defines local-id)]))])
      (cons (ILSimpleProvide *quoted-binding-ident-name*)
            (~> (unbox provides)
                (filter current-module-define? _)
                (remove-duplicates _)))))

  ;; Also expose quoted bindings
  (: quoted-bindings-stms ILStatement*)
  (define quoted-bindings-stms
    (cons (ILVarDec *quoted-binding-ident-name* (ILObject '()))
          (~> (set->list quoted-bindings)
              (filter (λ (b) (set-member? top-level-defines b)) _)
              (map (λ ([b : Symbol])
                     (ILAssign (ILRef *quoted-binding-ident-name* b) b))
                   _))))

  (ILModule path
            final-provides
            (append racket-requires (unbox js-requires))
            (append mod-stms quoted-bindings-stms)))


(: absyn-gtl-form->il (-> GeneralTopLevelForm ILStatement*))
(define (absyn-gtl-form->il form)
  (cond
    [(Expr? form)
     (define-values (stms v) (absyn-expr->il form #f))
     (append1 stms v)]
    [(DefineValues? form)
     (match-define (DefineValues ids expr) form)
     (absyn-binding->il (cons ids expr))]
    [(JSRequire? form) (error 'absyn-gtl-form->il
                              "Required should be hoisted")]))

(: absyn-provide->il (-> Provide* (Setof Symbol) ILProvide*))
(define (absyn-provide->il forms top-level-defs)
  (: defs-without-exclude (-> (Setof Symbol) (Setof Symbol) (Listof Symbol)))
  (define (defs-without-exclude def-set exclude-lst)
    (~> (set->list def-set)
        (filter (λ (id) (not (set-member? exclude-lst id))) _)))

  (for/fold ([result : ILProvide* '()])
            ([form forms])
    (match form
      [(SimpleProvide _) (cons form result)]
      [(RenamedProvide _ _) (cons form result)]
      [(AllDefined exclude)
       (~> (defs-without-exclude top-level-defs exclude)
           (map SimpleProvide _)
           (append result _))]
      [(PrefixAllDefined prefix-id exclude)
       (let* ([to-export (defs-without-exclude top-level-defs exclude)]
              [new-ids (map (λ (id)
                              (format-symbol "~a~a" prefix-id id))
                            to-export)])
         (append result
                 (map RenamedProvide to-export new-ids)))])))

(: absyn-expr->il (-> Expr Boolean (Values ILStatement* ILExpr)))
;;; An expression in Racket may need to be split into several
;;; statements in JS. However, since expression always has a
;;; values, we return pair of statements and the final value
;;; of expression.
;;; TODO: returned ILExpr should be just ILValue?
(define (absyn-expr->il expr overwrite-mark-frame?)
  (match expr
    [(PlainLambda formals body)
     ;; TODO: This is terribly mixed up lower level details. Perhaps
     ;;       something with IL can be improved this avoid this
     ;;       madness?
     (: ->jslist (-> ILExpr ILExpr))
     (define (->jslist f)
       (ILApp (name-in-module 'core 'Pair.listFromArray) (list f)))

     (define arguments-array
       (ILApp (name-in-module 'core 'argumentsToArray)
              (list (ILArguments))))

     (define-values (il-formals stms-formals-init)
       (cond
         [(symbol? formals)
          (values '()
                  (list
                   (ILVarDec formals (->jslist arguments-array))))]
         [(list? formals) (values formals '())]
         [(cons? formals)
          (define fi (car formals))
          (define fp (cdr formals))
          (define fi-len (length fi))
          (values fi
                  (list (ILVarDec fp
                                  (->jslist
                                   (ILApp
                                    (name-in-module 'core 'argumentsSlice)
                                    (list arguments-array
                                          (ILValue fi-len)))))))]))

     (define-values (body-stms body-value)
       (for/fold/last ([stms : ILStatement* '()]
                       [rv : ILExpr (ILValue (void))])
                      ([e last? body])
                      (define-values (s v)
                        (absyn-expr->il e (and last? overwrite-mark-frame?)))
                      (if last?
                          (values (append stms s) v)
                          (values (append stms s (list v)) v))))

     (define variadic-lambda? (not (list? formals)))
     (define lambda-expr (ILLambda il-formals
                                   (append stms-formals-init
                                           body-stms
                                           (list (ILReturn body-value)))))
     (values '()
             (if variadic-lambda?
                 (ILApp (name-in-module 'core 'attachProcedureArity)
                        (list lambda-expr))
                 lambda-expr))]

    [(CaseLambda clauses)
     (define-values (fixed-lams rest-lams)
       (if (case-lambda-has-dead-clause? expr)
           (begin (log-rjs-warning "found a case-lambda with dead-clause")
                  (values '() clauses))
           (values (filter (λ ([l : PlainLambda])
                             (number? (lambda-arity l)))
                           clauses)
                   (filter (λ ([l : PlainLambda])
                             (not (number? (lambda-arity l))))
                           clauses))))

     (define-values (stms val) (expand-normal-case-lambda fixed-lams rest-lams))

     (define arities
       (ILArray
        (for/list ([c (resolve-procedure-arities (map lambda-arity clauses))])
          (match c
            [(arity-at-least v)
             (define-values (_ val)
               (absyn-expr->il
                (PlainApp (ImportedIdent 'make-arity-at-least '#%kernel #t) (list (Quote v)))
                #f))
             val]
            [(? number? val) (ILValue val)]))))

     (values stms (ILApp (name-in-module 'core 'attachProcedureArity) (list val arities)))]

    [(If pred-e t-branch f-branch)
     (define-values (ps pe) (absyn-expr->il pred-e #f))
     (define-values (ts te) (absyn-expr->il t-branch overwrite-mark-frame?))
     (define-values (fs fe) (absyn-expr->il f-branch overwrite-mark-frame?))
     (define result-id (fresh-id 'if_res))
     (values (append ps
                     (list
                      (ILIf (ILBinaryOp '!==  (list pe (ILValue #f)))
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
                    (define-values (s nv)
                      (absyn-expr->il e (and last? overwrite-mark-frame?)))
                    (if last?
                        (values (append stms s) nv)
                        (values (append stms s (list nv)) nv)))]

    [(Set! id e)
     (values (let-values ([(stms v) (absyn-expr->il e #f)])
               (append1 stms
                        (ILAssign id v)))
             (ILValue (void)))]

    [(PlainApp (ImportedIdent '#%js-ffi _ _) args)
     (match args
       [(list (Quote 'var) (Quote var))
        (values '() (cast var Symbol))]
       [(list (Quote 'string) (Quote str))
        (values '() (ILValue str))]
       [(list (Quote 'ref) b xs ...)
        (define-values (stms il) (absyn-expr->il b #f))
        (values stms
                (for/fold ([il il])
                          ([x xs])
                  (match-define (Quote s) x) ;; Previous phase wrap
                                             ;; the symbol in quote

                  ;; HACK: To avoid wrapping the base symbol into
                  ;; Symbol class We have to generate code context
                  ;; sensitively
                  ;; TODO: We should accept only vaild JS idents.
                  (if (ILValue? il)
                      (ILRef (cast (ILValue-v il) Symbol)
                             (cast s Symbol))
                      (ILRef il (cast s Symbol)))))]
       [(list (Quote 'index) b xs ...)
        (define-values (stms il) (absyn-expr->il b #f))
        (for/fold ([stms stms]
                   [il il])
                  ([x xs])
          (define-values (x-stms s-il) (absyn-expr->il x #f))
          (values (append stms x-stms)
                  (if (ILValue? il)
                      (ILIndex (cast (ILValue-v il) Symbol)
                               s-il)
                      (ILIndex il s-il))))]
       [(list (Quote 'assign) lv rv)
        (define-values (lv-stms lv-il) (absyn-expr->il lv #f))
        (define-values (rv-stms rv-il) (absyn-expr->il rv #f))
        (values (append lv-stms
                        rv-stms
                        (list (ILAssign (cast lv-il ILLValue) rv-il)))
                (ILValue (void)))]
       [(list (Quote 'new) lv)
        (define-values (stms il) (absyn-expr->il lv #f))
        (values stms
                (ILNew (cast il (U ILLValue ILApp))))]
       [(list (Quote 'array) items ...)
        (define-values (stms* items*)
          (for/fold ([stms : ILStatement* '()]
                     [vals : (Listof ILExpr) '()])
                    ([item items])
            (define-values (s* v*) (absyn-expr->il item #f))
            (values (append stms s*)
                    (append vals (list v*)))))
        (values stms*
                (ILArray items*))]
       [(list (Quote 'object) items ...)
        (define-values (keys vals) (split-at items (cast (/ (length items) 2)
                                                           Nonnegative-Integer)))
        (define-values (stms* items*)
          (for/fold ([stms : ILStatement* '()]
                     [kvs : (Listof (Pairof ObjectKey ILExpr)) '()])
                    ([k (cast keys (Listof Quote))]
                     [v (cast vals (Listof Expr))])
            (define-values (s* v*) (absyn-expr->il v #f))
            (values (append stms s*)
                    (append kvs (list (cons (cast (Quote-datum k) ObjectKey)
                                            v*))))))
        (values stms* (ILObject items*))]
       [(list (Quote 'throw) e)
        (define-values (stms val) (absyn-expr->il e #f))
        (values (append1 stms (ILThrow val)) (ILValue (void)))]
       [(list (Quote 'typeof) e)
        (define-values (stms val) (absyn-expr->il e #f))
        (values stms (ILTypeOf val))]
       [(list (Quote 'instanceof) e t)
        ;;TODO: Not ANF.
        (define-values (stms val) (absyn-expr->il e #f))
        (define-values (tstms tval) (absyn-expr->il t #f))
        (values (append stms tstms) (ILInstanceOf val tval))]
       [(list (Quote 'operator) (Quote oper) e0 e1)
        ;;TODO: not ANF
        (define-values (stms0 val0) (absyn-expr->il e0 #f))
        (define-values (stms1 val1) (absyn-expr->il e1 #f))
        (values (append stms0 stms1)
                (ILBinaryOp (cast oper Symbol) (list val0 val1)))]
       [(list (Quote 'null))
        (values '() (ILNull))]
       [(list (Quote 'undefined))
        (values '() (ILUndefined))]
       [(list (Quote 'arguments))
        (values '() (ILArguments))]
       [(list (Quote 'this))
        (values '() (ILThis))]
       [_ (error 'absyn-expr->il "unknown ffi form" args)])]

    [(PlainApp lam args)
     ;;NOTE: Comparision operators work only on two operands TODO
     ;;later
     (: binops (Listof ImportedIdent))
     (define binops (map (λ ([b : Symbol])
                           (ImportedIdent b '#%kernel #t))
                         '(+ - * /)))

     (: il-app/binop (-> Ident (Listof ILExpr) (U ILApp ILBinaryOp)))
     (define (il-app/binop v arg*)
       (define v-il (let-values ([(_ v) (absyn-expr->il v #f)])
                      v))
       (cond
         [(and (equal? v (ImportedIdent '- '#%kernel #t))
               (length=? arg* 1))
          (ILApp v-il arg*)]
         [(and (equal? v (ImportedIdent '/ '#%kernel #t))
               (length=? arg* 1))
          (ILBinaryOp '/ (cons (ILValue 1) arg*))]
         [(and (ImportedIdent? v) (member v binops))
          (ILBinaryOp (ImportedIdent-id v) arg*)]
         [else (ILApp v-il arg*)]))

     ;; If some arguements produce statement, it may have side effects
     ;; and hence lambda expression should be computed first.
     (match-define-values (lam+arg-stms (cons lam-val arg-vals) _)
       (for/fold/last ([stms : ILStatement* '()]
                       [vals : (Listof ILExpr) '()]
                       [next-has-stms? : Boolean #f])
                      ([arg last? (reverse (cons lam args))])
         (define-values (s v) (absyn-expr->il arg #f))
         (cond
           [(and next-has-stms? (has-application? v))
            ;; If this argument expression has function application,
            ;; it may have side-effect hence we have to compute it
            ;; first.
            (define temp-id (fresh-id (if last? 'lam 'temp)))
            (define arg-stms (append1 s (ILVarDec temp-id v))) ;:TODO: use LetDecq
            (values (append arg-stms stms)
                    (cons temp-id vals)
                    next-has-stms?)]
           [next-has-stms?
            (values (append s stms)
                    (cons v vals)
                    next-has-stms?)]
           [else
            (values (append s stms)
                    (cons v vals)
                    (or next-has-stms? (cons? s)))])))

     (if (Ident? lam)
         (values lam+arg-stms
                 (il-app/binop lam arg-vals))
         (values lam+arg-stms
                 (ILApp lam-val arg-vals)))]

    [(TopId id) (values '() id)] ;; FIXME: rename top-levels?

    [(Quote datum) (values '() (absyn-value->il datum))]
    ;; Begin Statements

    [(cons hd '())
     (cond
       [(Expr? hd) (absyn-expr->il hd overwrite-mark-frame?)]
       [else (error "last datum in body must be expression")])]

    [(cons hd tl)
     (define hd-stms (absyn-top-level->il hd))
     (define-values (tl-stms v) (absyn-expr->il tl overwrite-mark-frame?))
     (values (append hd-stms tl-stms)
             v)]

    ['() (values '() (ILValue (void)))]

    [(LocalIdent id) (values '() id)]

    [(TopLevelIdent id) (values '() id)]

    [(Begin0 expr0 expr*)
     (define expr0-id (fresh-id 'begin-res))
     (absyn-expr->il
      (LetValues (list (cons `(,expr0-id) expr0))
                 (append1 expr* (LocalIdent expr0-id)))
      overwrite-mark-frame?)]

    [(ImportedIdent id src reachable?)
     ;; Imported Identifiers are compiled to a ref operation from the
     ;; module object. We do normalize the field we are looking for in
     ;; ILRef but only if its not a valid JavaScript id literal, which
     ;; excludes reserved keywords in this case.
     ;;
     ;; Hence, we have to normalize those cases right here so that we
     ;; refer to right binding.
     (define id* (if (reserved-keyword? id)
                     (string->symbol (normalize-symbol id))
                     id))

     ;; For compiling #%kernel (or primitive module) we may end
     ;; up thinking that's id is imported as we are actually
     ;; overriding the module. Don't make it happen.
     ;; TODO: Look at this again.
     (cond
       [(and (or (symbol? (current-source-file))
                 (path? (current-source-file)))
             (equal? (actual-module-path (cast (current-source-file) ModuleName))
                     (actual-module-path (cast src ModuleName))))
        (absyn-expr->il (LocalIdent id) overwrite-mark-frame?)]
       [(false? reachable?)
        ;; Probably a macro-introduced binding.
        ;; TODO: If its unimplemented primitive, we reach here. For
        ;;   primitive modules, filter unimplemented bindings.
        (define mod-obj-name (hash-ref (module-object-name-map) src))
        (values '()
                (ILRef (ILRef (assert mod-obj-name symbol?)
                              *quoted-binding-ident-name*)
                       id*))]
       [else
        (define mod-obj-name (hash-ref (module-object-name-map) src))
        (values '() (ILRef (assert mod-obj-name symbol?) id*))])]

    [(WithContinuationMark key _ (and (WithContinuationMark key _ _) wcm))
     ;; Overwrites previous key
     (absyn-expr->il wcm overwrite-mark-frame?)]

    [(WithContinuationMark key value result)
     (define-values (key-stms key-expr) (absyn-expr->il key #f))
     (define-values (value-stms value-expr) (absyn-expr->il value #f))
     (define-values (result-stms result-expr) (absyn-expr->il result #t))

     (define old-context-id (fresh-id '__context))
     (define new-context-id (fresh-id '__context))
     (define result-id (fresh-id '__wcm_result))
     (define exn-id (fresh-id 'exn))

     (define stms
        (list
         (ILVarDec old-context-id
                   (ILApp (name-in-module 'core 'Marks.getFrames) '()))
         (ILVarDec new-context-id #f)
         (ILExnHandler
          #;Try
          (flatten-statements
           (list key-stms
                 value-stms
                 (if overwrite-mark-frame?
                     (ILAssign new-context-id old-context-id)
                     (ILAssign new-context-id
                               (ILApp (name-in-module 'core 'Marks.enterFrame) '())))
                 (ILApp (name-in-module 'core 'Marks.setMark)
                        (list key-expr value-expr))
                 result-stms
                 (ILVarDec result-id result-expr)))
          #;Catch
          'exn
          '()
          #;Finally
          (list (ILApp (name-in-module 'core 'Marks.updateFrame)
                       (list old-context-id new-context-id))))))
     (values stms result-id)]

    [_ (error (~a "unsupported expr " expr))]))


(: absyn-binding->il (-> Binding ILStatement*))
(define (absyn-binding->il b)
  (match-define (cons args expr) b)
  (define-values (stms v) (absyn-expr->il expr #f))
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
         (ILVarDec arg (ILApp (ILRef result-id 'getAt)
                              (list (ILValue i))))))
     (append stms
             (cons (ILVarDec result-id v)
                   binding-stms))]))


(: absyn-value->il (-> Any ILExpr))
(define (absyn-value->il d)
  (cond
    [(Quote? d) (absyn-value->il (Quote-datum d))]
    [(string? d)
     (ILApp (name-in-module 'core 'UString.makeInternedImmutable)
            (list (ILValue d)))]
    [(symbol? d)
     (ILApp (name-in-module 'core 'Symbol.make)
            (list (ILValue (symbol->string d))))]
    [(keyword? d)
     (ILApp (name-in-module 'core 'Keyword.make)
            (list (ILValue (keyword->string d))))]
    [(list? d)
     (ILApp (name-in-module 'core 'Pair.makeList)
            (map absyn-value->il d))]
    [(empty? d)
     (name-in-module 'core 'Pair.EMPTY)]
    [(cons? d)
     (ILApp (name-in-module 'core 'Pair.make)
            (list (absyn-value->il (car d))
                  (absyn-value->il (cdr d))))]
    [(vector? d)
     (ILApp (name-in-module 'core 'Vector.make)
            (list
             (ILArray
              (map absyn-value->il
                   (vector->list (cast d (Vectorof Any)))))))]
    [(hash? d)
     (: maker Symbol)
     (define maker (cond
                     [(hash-eq? d) 'Hash.makeEq]
                     [(hash-eqv? d) 'Hash.makeEqv]
                     [(hash-equal? d) 'Hash.makeEqual]
                     [else (error 'assemble-value "unknown hash type")]))
     (define mutable (not (immutable? d)))
     (ILApp (name-in-module 'core maker)
            (list
             (ILArray
              (for/list ([(key value) (in-hash d)])
                (ILArray (list (absyn-value->il key) (absyn-value->il value)))))
             (ILValue mutable)))]
    [(bytes? d)
     (ILApp (name-in-module 'core 'Bytes.fromIntArray)
            (list (ILArray (map ILValue (bytes->list d)))))]
    [(box? d)
     (ILApp (name-in-module 'core 'Box.make)
            (list (absyn-value->il (unbox d))))]
    [(char? d)
     (ILApp (name-in-module 'core 'Char.charFromCodepoint)
            (list (absyn-value->il (char->integer d))))]
    [(or (integer? d)
         (boolean? d)
         (regexp? d)
         (byte-regexp? d)
         (void? d)
         (real? d))
     (ILValue d)]
    [else (error (~a "unsupported value" d))]))

(: expand-normal-case-lambda (-> (Listof PlainLambda)
                                 (Listof PlainLambda)
                                 (Values ILStatement* ILExpr)))
(define (expand-normal-case-lambda fixed-lams rest-lams)
  (define fixed-lam-names : (Listof Symbol)
    (build-list (length fixed-lams) (λ (_) (fresh-id 'cl))))
  (define rest-lam-names : (Listof Symbol)
    (build-list (length rest-lams) (λ (_) (fresh-id 'cl))))

  (define fixed-lam-name (fresh-id 'fixed-lam))
  (define fixed-lam-map
    (ILObject (map (λ ([id : Symbol] [lam : PlainLambda])
                     (let ([arity (lambda-arity lam)])
                       (if (number? arity)
                           (cons (string->symbol (~a arity)) id)
                           (error 'expand-normal-case-lambda "assertion failed"))))
                   fixed-lam-names fixed-lams)))

  (define *null* (PlainApp (ImportedIdent '#%js-ffi '#%kernel #t) (list (Quote 'null))))
  (define *arguments* (PlainApp (ImportedIdent '#%js-ffi '#%kernel #t) (list (Quote 'arguments))))

  (define-values (var-lam-stms var-lam-val)
    (absyn-expr->il
     (let loop : Expr ([lams*   rest-lams]
                       [names*  rest-lam-names])
          (match (list lams* names*)
            [(list '() '()) (PlainApp (ImportedIdent 'error '#%kernel #t)
                                      (list (Quote "case-lambda: invalid case")))]
            [(list (cons lh lt) (cons nh nt))
             (define arguments-length (PlainApp (ImportedIdent '#%js-ffi '#%kernel #t)
                                                (list (Quote 'ref)
                                                      (LocalIdent nh)
                                                      (Quote 'length))))
             (If ((get-formals-predicate lh) arguments-length)
                 (PlainApp (PlainApp (ImportedIdent '#%js-ffi '#%kernel #t)
                                     (list (Quote 'ref)
                                           (LocalIdent nh)
                                           (Quote 'apply)))
                           (list *null* *arguments*))
                 (loop lt nt))]))
     #f))

  ;; TODO: We can avoid using apply here.
  (define dispatch-stms
    (list (ILVarDec fixed-lam-name (ILIndex fixed-lam-map (ILRef (ILArguments) 'length)))
          (ILIf (ILBinaryOp '!== (list fixed-lam-name (ILUndefined)))
                (list (ILReturn
                       (ILApp (ILRef fixed-lam-name 'apply)
                              (list (ILNull) (ILArguments)))))
                (append1 var-lam-stms
                         (ILReturn var-lam-val)))))

  (let ([make-lam (λ ([id : Symbol] [lam : PlainLambda])
                    (define-values (stms lam-expr) (absyn-expr->il lam #f))
                    (assert stms empty?)
                    (ILVarDec id lam-expr))])
    (values (append (map make-lam fixed-lam-names fixed-lams)
                    (map make-lam rest-lam-names rest-lams))
            (ILLambda '() dispatch-stms))))

(: case-lambda-has-dead-clause? (-> CaseLambda Boolean))
;; Returns true if the given case-lambda has an unreachable clause.
;; Eg. (case-lambda
;;       [(a . b) "take at-least 1"]
;;       [(a b) "take exactly 2"])
(define (case-lambda-has-dead-clause? clam)
  (define-values (result _)
    (for/fold : (Values Boolean Number)
              ([res : Boolean #f]
               [least-var-arity : Real +inf.0])
              ([lam (CaseLambda-clauses clam)])
      (let ([arity (lambda-arity lam)])
        (cond
          [(and (number? arity) (< arity least-var-arity))
           (values res least-var-arity)]
          [(number? arity)
           (values #t least-var-arity)]
          [(and (arity-at-least? arity)
                (< (arity-at-least-value arity) least-var-arity))
           (values res (arity-at-least-value arity))]
          [(arity-at-least? arity)
           (values #t least-var-arity)]))))
  result)
(module+ test
  (check-false (case-lambda-has-dead-clause?
                (CaseLambda
                 (list
                  (PlainLambda '(a) '())
                  (PlainLambda '(a b) '())
                  (PlainLambda '((a) . c) '()))))
               "all clauses are reachable")
  (check-false (case-lambda-has-dead-clause?
                (CaseLambda
                 (list
                  (PlainLambda '((a b c) . d) '())
                  (PlainLambda '(a b) '()))))
               "all clauses are reachable")
  (check-true (case-lambda-has-dead-clause?
               (CaseLambda
                (list
                 (PlainLambda '((a b c) . d) '())
                 (PlainLambda '(a b c) '()))))
              "last clauses is unreachable")
  (check-true (case-lambda-has-dead-clause?
               (CaseLambda
                (list
                 (PlainLambda '(() . a) '())
                 (PlainLambda '((a) . c) '())
                 (PlainLambda '(a b) '()))))
              "second and third clause are unreachable")
  (check-true (case-lambda-has-dead-clause?
               (CaseLambda
                (list
                 (PlainLambda '(a) '())
                 (PlainLambda '((a) . c) '())
                 (PlainLambda '(a b) '()))))
              "third clause is unreachable"))

(: get-formals-predicate (-> PlainLambda (-> Expr Expr)))
(define (get-formals-predicate c)
  ;; TODO: Use binary ops instead of function call for cmp
  (define frmls (PlainLambda-formals c))
  (define length-js-name (ImportedIdent 'length '#%kernel #t))
  (cond
    [(symbol? frmls) (λ (_) (Quote #t))]
    [(list? frmls)
     (λ ([v : Expr])
       (PlainApp (ImportedIdent 'equal? '#%kernel #t)
                 (list
                  v
                  (Quote (length frmls)))))]
    [(cons? frmls)
     (λ ([v : Expr])
       (PlainApp (ImportedIdent '>= '#%kernel #t)
                 (list
                  v
                  (Quote (sub1
                          (length
                           (improper->proper frmls)))))))]))

(define-type ProcedureArity (U arity-at-least Exact-Nonnegative-Integer))
(: resolve-procedure-arities (-> (Listof ProcedureArity)
                                 (Listof ProcedureArity)))
(define (resolve-procedure-arities arities)
  (: arity< (-> ProcedureArity ProcedureArity Boolean))
  (define (arity< a b)
    (< (if (arity-at-least? a)
           (arity-at-least-value a)
           a)
       (if (arity-at-least? b)
           (arity-at-least-value b)
           b)))

  (: arity-equal? (-> ProcedureArity ProcedureArity Boolean))
  (define (arity-equal? a b)
    (and (not (arity< a b))
         (not (arity< b a))))

  (let loop ([arities* (sort arities arity<)]
             [result : (Listof ProcedureArity) '()])
    (match arities*
      ['() (reverse result)]
      [(cons (and (arity-at-least v) hd) tl)
       (reverse (if (and (cons? result) (arity-equal? (car result) hd))
                    (cons hd (cdr result))
                    (cons hd result)))]
      [(cons hd tl)
       (loop tl (if (and (cons? result) (arity-equal? (car result) hd))
                    result
                    (cons hd result)))])))

(module+ test
  (require typed/rackunit
           racket/pretty)

  (: -absyn-expr->il (-> Expr (Values ILStatement* ILExpr)))
  (define (-absyn-expr->il expr)
    (absyn-expr->il expr #f))

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
    (ilcheck -absyn-expr->il expr stms val))

  (define-syntax-rule (check-iltoplevel form stms)
    (ilcheck absyn-top-level->il form stms))

  (define-syntax-rule (check-ilgtl form stms)
    (ilcheck absyn-gtl-form->il form stms))

  (define LI LocalIdent)
  (define LI* (λ s*
                ((inst map LocalIdent Symbol)
                 LocalIdent
                 (cast s* (Listof Symbol)))))

  (: kident (-> Symbol ImportedIdent))
  (define (kident s) (ImportedIdent s '#%kernel #t))

  (define-syntax-rule (convert+print fn absyn)
    (parameterize ([fresh-id-counter 0]
                   [module-object-name-map
                    (hash '#%kernel 'kernel)])
      (pretty-print
       (values->list (fn absyn)))))

  ;; enable test environment
  (test-environment? #t)

  (: ~str (-> String ILExpr))
  (define (~str s)
    (ILApp
     (name-in-module 'core 'UString.makeInternedImmutable)
     (list (ILValue s))))

  (: ~sym (-> Symbol ILExpr))
  (define (~sym s)
    (ILApp
     (name-in-module 'core 'Symbol.make) (list (ILValue (symbol->string s)))))

  (: ~cons (-> ILExpr ILExpr ILExpr))
  (define (~cons a b)
    (ILApp (name-in-module 'core 'Pair.make) (list a b)))

  (define ~val ILValue)

  (define-syntax-rule (~list v ...)
    (ILApp (name-in-module 'core 'Pair.makeList) (list v ...)))

  ;; Expressions ----------------------------------------------------

  (test-case "Racket values"
    (check-ilexpr (Quote 42)        '() (~val 42))
    (check-ilexpr (Quote "Hello")   '() (~str "Hello"))
    (check-ilexpr (Quote 'hello)    '() (~sym 'hello))
    (check-ilexpr
     (Quote '(1 2 3))
     '()
     (~list (~val 1) (~val 2) (~val 3)))
    (check-ilexpr
     (Quote '(1 (2 3) 4 (a b)))
     '()
     (~list
      (~val 1)
      (~list (~val 2) (~val 3))
      (~val 4)
      (~list (~sym 'a) (~sym 'b))))
    (check-ilexpr (Quote '(1 . 2))  '() (~cons (~val 1) (~val 2)))
    (check-ilexpr (Quote #f)        '() (~val #f)))


  ;; --------------------------------------------------------------------------

  (test-case "Function Application"
    (check-ilexpr (PlainLambda '(x) (LI* 'x))
                  '()
                  (ILLambda '(x) (list (ILReturn 'x))))
    (check-ilexpr (PlainLambda 'x (LI* 'x))
                  '()
                  (ILApp
                   (name-in-module 'core 'attachProcedureArity)
                   (list
                    (ILLambda
                     '()
                     (list
                      (ILVarDec
                       'x
                       (ILApp
                        (name-in-module 'core 'Pair.listFromArray)
                        (list
                         (ILApp (name-in-module 'core 'argumentsToArray)
                                (list (ILArguments))))))
                      (ILReturn 'x)))))))

  ;; --------------------------------------------------------------------------

  (test-case "If expression"
    (check-ilexpr (If (Quote #t) (Quote 'yes) (Quote 'no))
                  (list
                   (ILIf (ILBinaryOp '!== (list (ILValue #t) (ILValue #f)))
                         (list (ILVarDec 'if_res1 (~sym 'yes)))
                         (list (ILVarDec 'if_res1 (~sym 'no)))))
                  'if_res1))

  ;; --------------------------------------------------------------------------

  (test-case "Lambda expressions"
    (check-ilexpr (PlainLambda '(x) (LI* 'x))
                  '()
                  (ILLambda
                   '(x)
                   (list (ILReturn 'x))))
    (check-ilexpr (PlainLambda '(a b)
                               (list
                                (PlainApp (LocalIdent 'list)
                                          (LI* 'a 'b))))
                  '()
                  (ILLambda
                   '(a b)
                   (list
                    (ILReturn (ILApp 'list '(a b)))))))


  (test-case "Let expressions"
    (check-ilexpr (LetValues (list (cons '(a) (Quote 1))
                                   (cons '(b) (Quote 2)))
                             (LI* 'a 'b))
                  (list
                   (ILVarDec 'a (ILValue 1)) (ILVarDec 'b (ILValue 2))
                   'a)
                  'b)
    (check-ilexpr (LetValues (list (cons '(a)
                                         (If (Quote #t)
                                             (Quote 'yes)
                                             (Quote 'false)))
                                   (cons '(b)
                                         (PlainApp (kident '+)
                                                   (list
                                                    (Quote 1)
                                                    (Quote 2)))))
                             (list (PlainApp (LocalIdent 'list)
                                             (LI* 'a 'b))))
                  (list
                   (ILIf (ILBinaryOp '!== (list (~val #t) (~val #f)))
                         (list (ILVarDec 'if_res1 (~sym 'yes)))
                         (list (ILVarDec 'if_res1 (~sym 'false))))
                   (ILVarDec 'a 'if_res1)
                   (ILVarDec 'b (ILBinaryOp '+ (list (~val 1) (~val 2)))))
                  (ILApp 'list '(a b))))

  ;; --------------------------------------------------------------------------

  (test-case "Identify binary operators"
    (check-ilexpr (PlainApp (kident '+) (list (Quote 1) (Quote 2)))
                  '()
                  (ILBinaryOp '+ (list (ILValue 1) (ILValue 2))))
    (check-ilexpr (PlainApp (kident '-) (list (Quote 1)
                                              (Quote 2)
                                              (Quote 3)))
                  '()
                  (ILBinaryOp '-
                              (list
                               (ILValue 1) (ILValue 2) (ILValue 3)))))

  ;; --------------------------------------------------------------------------

  (test-case "Case Lambda"
    (check-equal? (resolve-procedure-arities '(1 2 3 4))
                  '(1 2 3 4))
    (check-equal? (resolve-procedure-arities '(1 3 2 6 4))
                  '(1 2 3 4 6))
    (check-equal? (resolve-procedure-arities
                   (list 1 2 3 (arity-at-least 3) (arity-at-least 6)))
                  (list 1 2 (arity-at-least 3)))
    (check-equal? (resolve-procedure-arities
                   (list 1 2 3 (arity-at-least 6) (arity-at-least 3)))
                  (list 1 2 (arity-at-least 3)))

    (check-ilexpr
     (CaseLambda
      (list
       (PlainLambda '(a b) (list (PlainApp (kident 'add)
                                           (LI* 'a 'b))))
       (PlainLambda '(a b c) (list (PlainApp (kident 'mul)
                                             (LI* 'a 'b 'c))))))
     (list
      (ILVarDec
       'cl1
       (ILLambda '(a b) (list (ILReturn (ILApp (ILRef 'kernel 'add) '(a b))))))
      (ILVarDec
       'cl2
       (ILLambda
        '(a b c)
        (list (ILReturn (ILApp (ILRef 'kernel 'mul) '(a b c)))))))
     (ILApp
      '$rjs_core.attachProcedureArity
      (list
       (ILLambda
        '()
        (list
         (ILVarDec
          'fixed-lam3
          (ILIndex
           (ILObject '((|2| . cl1) (|3| . cl2)))
           (ILRef (ILArguments) 'length)))
         (ILIf
          (ILBinaryOp '!== (list 'fixed-lam3 (ILUndefined)))
          (list
           (ILReturn
            (ILApp (ILRef 'fixed-lam3 'apply) (list (ILNull) (ILArguments)))))
          (list
           (ILReturn
            (ILApp
             (ILRef 'kernel 'error)
             (list (~str "case-lambda: invalid case"))))))))
       (ILArray (list (ILValue 2) (ILValue 3))))))


  ;; --------------------------------------------------------------------------
  (define absyn-js-ffi (ImportedIdent '#%js-ffi "fakepath.rkt" #t))

  (test-case "Foreign Function Interface"
    (check-ilexpr
     (PlainApp absyn-js-ffi (list (Quote 'var) (Quote 'console)))
     '()
     'console)

    (check-ilexpr
     (PlainApp
      absyn-js-ffi
      (list (Quote 'ref) (Quote 'obj) (Quote 'key1) (Quote 'key2)))
     '()
     (ILRef (ILRef (~sym 'obj) 'key1) 'key2))

    (check-ilexpr
     (PlainApp
      absyn-js-ffi
      (list (Quote 'ref)
            (LocalIdent 'obj) (Quote 'key1) (Quote 'key2)))
     '()
     (ILRef (ILRef 'obj 'key1) 'key2))

    (check-ilexpr
     (PlainApp
      absyn-js-ffi
      (list (Quote 'index)
            (Quote 'obj)
            (PlainApp (LocalIdent 'do) (list (Quote "Hello!")))))
     '()
     (ILIndex (~sym 'obj) (ILApp 'do (list (~str "Hello!")))))

    (check-ilexpr
     (PlainApp
      absyn-js-ffi
      (list (Quote 'object)
            (Quote 'key1) (Quote 'key2) (Quote 'key3)
            (Quote 'val1) (Quote 'val2) (Quote 'val3)))
     '()
     (ILObject (list (cons 'key1 (~sym 'val1))
                     (cons 'key2 (~sym 'val2))
                     (cons 'key3 (~sym 'val3)))))

    (check-ilexpr
     (PlainApp
      absyn-js-ffi
      (list (Quote 'array)
            (Quote 1) (Quote 2) (Quote 3)))
     '()
     (ILArray (list (ILValue 1) (ILValue 2) (ILValue 3))))

    (check-ilexpr
     (PlainApp
      absyn-js-ffi
      (list (Quote 'assign)
            (LocalIdent 'name) (Quote "John Doe")))
     (list (ILAssign 'name (~str "John Doe")))
     (ILValue (void))) ;;TODO: FFI special case!

    (check-ilexpr
     (PlainApp
      absyn-js-ffi
      (list (Quote 'new) (LocalIdent 'Array)))
     '()
     (ILNew 'Array))

    (check-ilexpr
     (PlainApp
      absyn-js-ffi
      (list (Quote 'throw) (Quote "What")))
     (list (ILThrow (~str "What")))
     (ILValue (void)))

    (check-ilexpr
     (PlainApp
      absyn-js-ffi
      (list (Quote 'operator) (Quote '+) (Quote 1) (Quote 2)))
     '()
     (ILBinaryOp '+ (list (ILValue 1) (ILValue 2)))))

  ;; --------------------------------------------------------------------------

  (test-case "Top Level Forms"
    (check-iltoplevel
     (list (PlainApp (kident 'display) (list (Quote "hello")))
           (PlainApp (kident 'print) (list (Quote "what" )
                                           (LocalIdent 'out))))
     (list
      (ILApp (ILRef 'kernel 'display) (list (~str "hello")))
      (ILApp (ILRef 'kernel 'print) (list (~str "what") 'out)))))

  ;; --------------------------------------------------------------------------

  (test-case "General Top Level Forms"
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
       (ILApp (ILRef 'kernel 'values)
              (list (ILValue 42) (ILLambda '(x) (list (ILReturn 'x))))))
      (ILVarDec 'x (ILApp (ILRef 'let_result1 'getAt) (list (ILValue 0))))
      (ILVarDec 'ident (ILApp (ILRef 'let_result1 'getAt) (list (ILValue 1)))))))))
