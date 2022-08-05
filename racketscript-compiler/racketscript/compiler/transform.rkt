#lang racket/base

;;; Generate IL code from abstract syntax. Each binding name
;;; in assumed to be fresh, to enforce lexical scope rules of Racket

(require racket/bool
         racket/list
         version/utils
         "ast.rkt"
         "config.rkt"
         "environment.rkt"
         "il-analyze.rkt"
         "ir.rkt"
         "logging.rkt"
         "util.rkt"
         "set.rkt"
         "match.rkt"
         "struct-match.rkt")

(provide absyn-top-level->il
         absyn-gtl-form->il
         absyn-expr->il
         absyn-linklet->il)

(define module-object-name-map (make-parameter (hash)))

(define (absyn-top-level->il form)
  (cond
    [(Linklet? form) (error 'absyn-top-level->il
                            "Not supported. Todo.")]
    [(Expr? form)
     (define-values (stmt v) (absyn-expr->il form #f))
     (append1 stmt v)]
    [(Begin? form) (absyn-expr->il form #f)]
    [else (error "only some forms supported at top level")]))

;; FIXME I do ZERO handling of javascript forms
(define (absyn-linklet->il lnk)
  (struct-match-define (Linklet path imports exports forms) lnk)
  (log-rjs-info "[linklet il]")

  (define imported-mod-path-list (flatten imports))
  (define requires* (absyn-requires->il imported-mod-path-list path))

  ;; FIXME it's really odd that we have three things that keep needing to get passed around that have the same information
  ;;       in them -- imported-mod-path-list + the 'requires' objects + module-object-name-map


  (parameterize ([module-object-name-map (make-module-map requires* imported-mod-path-list)])
    (ILLinklet
      (filter ILRequire? requires*)
      (absyn-exports->il exports)
      (append-map absyn-gtl-form->il forms))))

(define (make-module-map reqs imported-paths)
  (for/fold ([acc (module-object-name-map)])
            ([req reqs]
             [mod-path imported-paths])
    (if (ILRequire? req)
      (hash-set acc
                mod-path
                (ILRequire-name req))
      acc)))

(define (absyn-exports->il exports)
  (for/list ([ex exports])
    (cond
      [(symbol? ex) (SimpleProvide ex)]
      [(list? ex) (RenamedProvide (car ex) (cadr ex))]
      [else (error 'exports->il "unknown export form in linklet" ex)])))


;; TODO not using a lot of the code here, eventually should be able to delete
(define (absyn-requires->il import-list path)
  (for/list ([mod-path import-list]
             [counter (in-naturals)])
    (define mod-obj-name (string->symbol (~a "M" counter)))
    (ILRequire mod-path mod-obj-name '*)))
    ;; (define import-name
    ;;   (if (symbol? mod-path)
    ;;     (jsruntime-import-path path
    ;;                            (jsruntime-module-path mod-path))
    ;;     (module->relative-import mod-path)))

    ;; ;; See expansion of identifier in `expand.rkt` for primitive
    ;; ;; modules
    ;; (if (or (and (primitive-module? mod-path)  ;; a self-import cycle
    ;;               (equal? path (actual-module-path mod-path)))
    ;;         (and (primitive-module-path? (actual-module-path path))
    ;;               (set-member? ignored-module-imports-in-boot mod-path)))
    ;;     #f
    ;;     (ILRequire import-name mod-obj-name '*))))

(define (absyn-gtl-form->il form)
  (cond
    [(Expr? form)
     (define-values (stms v) (absyn-expr->il form #f))
     (append1 stms v)]
    [(DefineValues? form)
     (struct-match-define (DefineValues ids expr) form)
     (absyn-binding->il (cons ids expr))]
    [(JSRequire? form) (error 'absyn-gtl-form->il
                              "Required should be hoisted")]))

(define (absyn-provide->il forms top-level-defs)
  (define (defs-without-exclude def-set exclude-lst)
    (let ([def-lst (set->list def-set)])
      (filter (λ (id) (not (set-member? exclude-lst id))) def-lst)))

  (for/fold ([result '()])
            ([form forms])
    ;; TODO adding sufixes to underscored identifiers so I don't need to add
    ;;      support for them in my pattern matcher
    (struct-match form
      [(SimpleProvide _fst) (cons form result)]
      [(RenamedProvide _fst _snd) (cons form result)])))

;;; An expression in Racket may need to be split into several
;;; statements in JS. However, since expression always has a
;;; values, we return pair of statements and the final value
;;; of expression.
;;; TODO: returned ILExpr should be just ILValue?
(define (absyn-expr->il expr overwrite-mark-frame?)
  (struct-match expr
    [(Lambda formals body unchecked?)
     (define (->jslist f)
       (ILApp (name-in-module 'core 'Pair.listFromArray) (list f)))

     (define (maybe-make-checked-formals formals)
       (if (or unchecked? (skip-arity-checks?))
           formals
           (ILCheckedFormals formals)))

     (define-values (in-formals stms-formals-init)
       (cond
         [(symbol? formals)
          (define in-formals (fresh-id formals))
          (values in-formals (list (ILVarDec formals (->jslist in-formals))))]
         [(list? formals) (values formals '())]
         [(cons? formals)
          (define fi (car formals))
          (define fp (cdr formals))
          (define in-fp (fresh-id fp))
          (values (cons fi in-fp)
                  (list (ILVarDec fp (->jslist in-fp))))]))

     (define-values (body-stms body-value)
       (for/fold/last ([stms '()]
                       [rv (ILValue (void))])
                      ([e last? body])
                      (define-values (s v)
                        (absyn-expr->il e (and last? overwrite-mark-frame?)))
                      (if last?
                          (values (append stms s) v)
                          (values (append stms s (list v)) v))))

     (define lambda-expr (ILLambda (maybe-make-checked-formals in-formals)
                                   (append stms-formals-init
                                           body-stms
                                           (list (ILReturn body-value)))))
     (values '()
             (if (variadic-lambda? expr)
                 (ILApp (name-in-module 'core 'attachProcedureArity)
                        (list lambda-expr))
                 lambda-expr))]

    [(CaseLambda clauses)
     (define-values (fixed-lams rest-lams)
       (if (case-lambda-has-dead-clause? expr)
           (begin (log-rjs-warning "found a case-lambda with dead-clause")
                  (values '() clauses))
           (values (filter (λ (l)
                             (number? (lambda-arity l)))
                           clauses)
                   (filter (λ (l)
                             (not (number? (lambda-arity l))))
                           clauses))))

     (define-values (stms val) (expand-normal-case-lambda fixed-lams rest-lams))

     (define arities
       (ILArray
        (for/list ([c (resolve-procedure-arities (map lambda-arity clauses))])
          (cond
            [(arity-at-least? c)
             (define-values (_ val)
               (absyn-expr->il
                 (App (ImportedIdent 'make-arity-at-least '#%kernel #t) (list (Quote (arity-at-least-value c))))
                 #f))
             val]
            [(number? val) (ILValue val)]))))

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
       (for/fold ([stms '()])
                 ([b bindings])
         (append stms
                 (absyn-binding->il b))))
     (for/fold/last ([stms binding-stms]
                     [rv (ILValue (void))])
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

    [(App lam args)
     #:when (and (ImportedIdent? lam)
                 (eq? '#%js-ffi (ImportedIdent-id lam)))
     (match args
       [`(,fst ,snd)
        #:when (and (Quote? fst)
                    (Quote? snd)
                    (eq? (Quote-datum fst) 'var))
        (values '() (Quote-datum snd))]
       [`(,fst ,snd)
        #:when (and (Quote? fst)
                    (Quote? snd)
                    (eq? (Quote-datum fst) 'string))
        (values '() (ILValue (Quote-datum snd)))]
       [`(,fst ,b ,xs ...)
        #:when (and (Quote? fst)
                    (eq? (Quote-datum fst) 'ref))
        (define-values (stms il) (absyn-expr->il b #f))
        (values stms
                (for/fold ([il il])
                          ([x xs])
                  (struct-match-define (Quote s) x) ;; Previous phase wrap
                                             ;; the symbol in quote

                  ;; HACK: To avoid wrapping the base symbol into
                  ;; Symbol class We have to generate code context
                  ;; sensitively
                  ;; TODO: We should accept only vaild JS idents.
                  (if (ILValue? il)
                      (ILRef (ILValue-v il)
                             s)
                      (ILRef il s))))]
       [`(,fst ,b ,xs ...)
        #:when (and (Quote? fst)
                    (eq? (Quote-datum fst) 'index))
        (define-values (stms il) (absyn-expr->il b #f))
        (for/fold ([stms stms]
                   [il il])
                  ([x xs])
          (define-values (x-stms s-il) (absyn-expr->il x #f))
          (values (append stms x-stms)
                  (if (ILValue? il)
                      (ILIndex (ILValue-v il)
                               s-il)
                      (ILIndex il s-il))))]
       [`(,fst ,lv ,rv)
        #:when (and (Quote? fst)
                    (eq? (Quote-datum fst) 'assign))
        (define-values (lv-stms lv-il) (absyn-expr->il lv #f))
        (define-values (rv-stms rv-il) (absyn-expr->il rv #f))
        (values (append lv-stms
                        rv-stms
                        (list (ILAssign lv-il rv-il)))
                (ILValue (void)))]
       [`(,fst ,lv)
        #:when (and (Quote? fst)
                    (eq? (Quote-datum fst) 'new))
        (define-values (stms il) (absyn-expr->il lv #f))
        (values stms
                (ILNew il))]
       [`(,fst ,items ...)
        #:when (and (Quote? fst)
                    (eq? (Quote-datum fst) 'array))
        (define-values (stms* items*)
          (for/fold ([stms '()]
                     [vals '()])
                    ([item items])
            (define-values (s* v*) (absyn-expr->il item #f))
            (values (append stms s*)
                    (append vals (list v*)))))
        (values stms*
                (ILArray items*))]
       [`(,fst ,items ...)
        #:when (and (Quote? fst)
                    (eq? (Quote-datum fst) 'object))
        (define-values (keys vals) (split-at items (/ (length items) 2)))
        (define-values (stms* items*)
          (for/fold ([stms '()]
                     [kvs '()])
                    ([k keys]
                     [v vals])
            (define-values (s* v*) (absyn-expr->il v #f))
            (values (append stms s*)
                    (append kvs (list (cons (Quote-datum k)
                                            v*))))))
        (values stms* (ILObject items*))]
       [`(,fst ,e)
        #:when (and (Quote? fst)
                    (eq? (Quote-datum fst) 'throw)) 
        (define-values (stms val) (absyn-expr->il e #f))
        (values (append1 stms (ILThrow val)) (ILValue (void)))]
       [`(,fst ,e)
        #:when (and (Quote? fst)
                    (eq? (Quote-datum fst) 'typeof))
        (define-values (stms val) (absyn-expr->il e #f))
        (values stms (ILTypeOf val))]
       [`(,fst ,e ,t)
        #:when (and (Quote? fst)
                    (eq? (Quote-datum fst) 'instanceof))
        ;;TODO: Not ANF.
        (define-values (stms val) (absyn-expr->il e #f))
        (define-values (tstms tval) (absyn-expr->il t #f))
        (values (append stms tstms) (ILInstanceOf val tval))]
       [`(,fst ,snd ,e0 ,e1)
        #:when (and (Quote? fst)
                    (Quote? snd)
                    (eq? (Quote-datum fst) 'operator))
        (struct-match-define (Quote oper) snd)
        ;;TODO: not ANF
        (define-values (stms0 val0) (absyn-expr->il e0 #f))
        (define-values (stms1 val1) (absyn-expr->il e1 #f))
        (values (append stms0 stms1)
                (ILBinaryOp oper (list val0 val1)))]
       [`(,fst)
        #:when (and (Quote? fst)
                    (eq? (Quote-datum fst) 'null))
        (values '() (ILNull))]
       [`(,fst)
        #:when (and (Quote? fst)
                    (eq? (Quote-datum fst) 'undefined))
        (values '() (ILUndefined))]
       [`(,fst)
        #:when (and (Quote? fst)
                    (eq? (Quote-datum fst) 'arguments))
        (values '() (ILArguments))]
       [`(,fst)
        #:when (and (Quote? fst)
                    (eq? (Quote-datum fst) 'this))
        (values '() (ILThis))]
       [`,_ (error 'absyn-expr->il "unknown ffi form" args)])]

    [(App lam args)
     ;;NOTE: Comparision operators work only on two operands TODO
     ;;later
     (define binops (map (λ (b)
                           (ImportedIdent b '#%kernel #t))
                         '(+ - * /)))

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
         [(and (ImportedIdent? v) (member v binops) (>= (length arg*) 2))
          (ILBinaryOp (ImportedIdent-id v) arg*)]
         [else (ILApp v-il arg*)]))

     ;; If some arguements produce statement, it may have side effects
     ;; and hence lambda expression should be computed first.
     (define-values (lam+arg-stms lam-part-vals _rst)
       (for/fold/last ([stms '()]
                       [vals '()]
                       [next-has-stms? #f])
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

     (match-define `(,lam-val ,arg-vals ...) lam-part-vals)

     (if (Ident? lam)
         (values lam+arg-stms
                 (il-app/binop lam arg-vals))
         (values lam+arg-stms
                 (ILApp lam-val arg-vals)))]

    [(Quote datum) (values '() (absyn-value->il datum))]
    ;; Begin Statements

    [_ #:when (and (pair? expr) (null? (cdr expr)))
     (displayln "bar")
     (displayln expr)
     (match-define `(,hd) expr)
     (displayln "foo")
     (displayln hd)
     (cond
       [(Expr? hd) (absyn-expr->il hd overwrite-mark-frame?)]
       [else (error "last datum in body must be expression")])]

    [_ #:when (pair? expr)
     (match-define `(,hd . ,tl) expr)
     (define hd-stms (absyn-top-level->il hd))
     (define-values (tl-stms v) (absyn-expr->il tl overwrite-mark-frame?))
     (values (append hd-stms tl-stms)
             v)]

    [_ #:when (null? expr) (values '() (ILValue (void)))]

    [(LocalIdent id) (values '() id)]

    [(TopLevelIdent id) (values '() id)]

    [(Begin0 expr0 expr*)
     (define expr0-id (fresh-id 'begin-res))
     (absyn-expr->il
      (LetValues (list (cons `(,expr0-id) expr0))
                 (append1 expr* (LocalIdent expr0-id)))
      overwrite-mark-frame?)]

    [(ImportedIdent id src_ reachable?)
     ;; FIXME?: Racket7 workaround
     (define src
       (if (and (version<=? "7.0" (version))
                (equal? src_ '#%runtime))
           '#%kernel
           src_))

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
             (equal? (actual-module-path (current-source-file))
                     (actual-module-path src)))
        (absyn-expr->il (LocalIdent id) overwrite-mark-frame?)]
       [(false? reachable?)
        ;; Probably a macro-introduced binding.
        ;; TODO: If its unimplemented primitive, we reach here. For
        ;;   primitive modules, filter unimplemented bindings.
        (define mod-obj-name (hash-ref (module-object-name-map) src
                                       (lambda ()
                                         (log-rjs-warning "missing unreachable binding ~s ~s" id* src)
                                         src)))
        (values '()
                (ILRef (ILRef mod-obj-name
                              *quoted-binding-ident-name*)
                       id*))]
       [else
        (define mod-obj-name (hash-ref (module-object-name-map) src
                                       (lambda ()
                                         (log-rjs-warning "missing reachable binding ~s ~s" id* src)
                                         src)))
        (values '() (ILRef mod-obj-name id*))])]

    [(WithContinuationMark key _fst wcm) #:when (WithContinuationMark? wcm)
     (struct-match-define (WithContinuationMark key _snd _thd) wcm)

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
    [(VarRef _)  (values '() (absyn-value->il '#%variable-reference))]

    [_ (error (~a "unsupported expr " expr))]))


(define (absyn-binding->il b)
  (match-define `(,args . ,expr) b)
  (define-values (stms v) (absyn-expr->il expr #f))
  (match args
    [`(,a)
     (append1 stms
              (ILVarDec a v))]
    [`,_
     (define result-id (fresh-id 'let_result))
     (define binding-stms
       (for/list ([i (range (length args))]
                  [arg args])
         (ILVarDec arg (ILApp (ILRef result-id 'getAt)
                              (list (ILValue i))))))
     (append stms
             (cons (ILVarDec result-id v)
                   binding-stms))]))


(define (absyn-value->il d)
  (cond
    [(Quote? d) (absyn-value->il (Quote-datum d))]
    [(string? d)
     (ILApp (name-in-module 'core 'UString.make)
            (list (ILValue d)))]
    [(symbol? d)
     (ILApp (name-in-module 'core 'PrimitiveSymbol.make)
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
                   (vector->list d)))))]
    [(hash? d)
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
    [(or (regexp? d) (byte-regexp? d))
     (define v (object-name d)) ; string or bytes
     (ILApp (name-in-module 'core 'Regexp.fromString)
            (list (ILValue (if (bytes? v) (bytes->string/utf-8 v) v))))]
    [(or (integer? d)
         (boolean? d)
         (void? d)
         (real? d))
     (ILValue d)]
    [else (error (~a "unsupported value" d))]))

(define (expand-normal-case-lambda fixed-lams rest-lams)
  (define fixed-lam-names
    (build-list (length fixed-lams) (λ (_) (fresh-id 'cl))))
  (define rest-lam-names
    (build-list (length rest-lams) (λ (_) (fresh-id 'cl))))

  (define fixed-lam-name (fresh-id 'fixed-lam))
  (define fixed-lam-map
    (ILObject (map (λ (id lam)
                     (let ([arity (lambda-arity lam)])
                       (if (number? arity)
                           (cons (string->symbol (~a arity)) id)
                           (error 'expand-normal-case-lambda "assertion failed"))))
                   fixed-lam-names fixed-lams)))

  (define *null* (App (ImportedIdent '#%js-ffi '#%kernel #t) (list (Quote 'null))))
  (define *arguments* (App (ImportedIdent '#%js-ffi '#%kernel #t) (list (Quote 'arguments))))

  (define-values (var-lam-stms var-lam-val)
    (absyn-expr->il
     (let loop ([lams*   rest-lams]
                [names*  rest-lam-names])
          (match (list lams* names*)
            [`(() ()) (App (ImportedIdent 'error '#%kernel #t)
                           (list (Quote "case-lambda: invalid case")))]
            [`((,lh . ,lt) (,nh . ,nt))
             (define arguments-length (App (ImportedIdent '#%js-ffi '#%kernel #t)
                                                (list (Quote 'ref)
                                                      (LocalIdent nh)
                                                      (Quote 'length))))
             (If ((get-formals-predicate lh) arguments-length)
                 (App (App (ImportedIdent '#%js-ffi '#%kernel #t)
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

  (let ([make-lam (λ (id lam)
                    (define-values (stms lam-expr) (absyn-expr->il lam #f))
                    stms
                    (ILVarDec id lam-expr))])
    (values (append (map make-lam fixed-lam-names fixed-lams)
                    (map make-lam rest-lam-names rest-lams))
            (ILLambda '() dispatch-stms))))

;; Returns true if the given case-lambda has an unreachable clause.
;; Eg. (case-lambda
;;       [(a . b) "take at-least 1"]
;;       [(a b) "take exactly 2"])
(define (case-lambda-has-dead-clause? clam)
  (define-values (result _)
    (for/fold
      ([res #f]
       [least-var-arity +inf.0])
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
                  (Lambda '(a) '() #f)
                  (Lambda '(a b) '() #f)
                  (Lambda '((a) . c) '() #f))))
               "all clauses are reachable")
  (check-false (case-lambda-has-dead-clause?
                (CaseLambda
                 (list
                  (Lambda '((a b c) . d) '() #f)
                  (Lambda '(a b) '() #f))))
               "all clauses are reachable")
  (check-true (case-lambda-has-dead-clause?
               (CaseLambda
                (list
                 (Lambda '((a b c) . d) '() #f)
                 (Lambda '(a b c) '() #f))))
              "last clauses is unreachable")
  (check-true (case-lambda-has-dead-clause?
               (CaseLambda
                (list
                 (Lambda '(() . a) '() #f)
                 (Lambda '((a) . c) '() #f)
                 (Lambda '(a b) '() #f))))
              "second and third clause are unreachable")
  (check-true (case-lambda-has-dead-clause?
               (CaseLambda
                (list
                 (Lambda '(a) '() #f)
                 (Lambda '((a) . c) '() #f)
                 (Lambda '(a b) '() #f))))
              "third clause is unreachable"))

(define (get-formals-predicate c)
  ;; TODO: Use binary ops instead of function call for cmp
  (define frmls (Lambda-formals c))
  (define length-js-name (ImportedIdent 'length '#%kernel #t))
  (cond
    [(symbol? frmls) (λ (_) (Quote #t))]
    [(list? frmls)
     (λ (v)
       (App (ImportedIdent 'equal? '#%kernel #t)
                 (list
                  v
                  (Quote (length frmls)))))]
    [(cons? frmls)
     (λ (v)
       (App (ImportedIdent '>= '#%kernel #t)
                 (list
                  v
                  (Quote (sub1
                          (length
                           (improper->proper frmls)))))))]))

(define (resolve-procedure-arities arities)
  (define (arity< a b)
    (< (if (arity-at-least? a)
           (arity-at-least-value a)
           a)
       (if (arity-at-least? b)
           (arity-at-least-value b)
           b)))

  (define (arity-equal? a b)
    (and (not (arity< a b))
         (not (arity< b a))))

  (let loop ([arities* (sort arities arity<)]
             [result '()])
    (match arities*
      [`() (reverse result)]
      [`(,hd . ,tl) #:when (arity-at-least? hd)
       (reverse (if (and (cons? result) (arity-equal? (car result) hd))
                    (cons hd (cdr result))
                    (cons hd result)))]
      [`(,hd . ,tl)
       (loop tl (if (and (cons? result) (arity-equal? (car result) hd))
                    result
                    (cons hd result)))])))

(module+ test
  (require rackunit)

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
  (define LI* (λ s* (map LocalIdent s*)))

  (define (kident s) (ImportedIdent s '#%kernel #t))

  (define-syntax-rule (convert+print fn absyn)
    (parameterize ([fresh-id-counter 0]
                   [module-object-name-map
                    (hash '#%kernel 'kernel)])
      (pretty-print
       (values->list (fn absyn)))))

  ;; enable test environment
  (test-environment? #t)

  (define (~str s)
    (ILApp
     (name-in-module 'core 'UString.make)
     (list (ILValue s))))

  (define (~sym s)
    (ILApp
     (name-in-module 'core 'PrimitiveSymbol.make) (list (ILValue (symbol->string s)))))

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

  (test-case "Lambda Expressions"
    (check-ilexpr (Lambda '(x) (LI* 'x) #t)
                  '()
                  (ILLambda '(x) (list (ILReturn 'x))))
    (check-ilexpr (Lambda 'x (LI* 'x) #t)
                  '()
                  (ILApp
                   (name-in-module 'core 'attachProcedureArity)
                   (list
                    (ILLambda
                     'x1
                     (list
                      (ILVarDec
                       'x
                       (ILApp
                        (name-in-module 'core 'Pair.listFromArray)
                        (list 'x1)))
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
    (check-ilexpr (Lambda '(x) (LI* 'x) #t)
                  '()
                  (ILLambda
                   '(x)
                   (list (ILReturn 'x))))
    (check-ilexpr (Lambda '(a b)
                               (list
                                (App (LocalIdent 'list)
                                          (LI* 'a 'b)))
                               #t)
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
                                         (App (kident '+)
                                                   (list
                                                    (Quote 1)
                                                    (Quote 2)))))
                             (list (App (LocalIdent 'list)
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
    (check-ilexpr (App (kident '+) '())
                  '()
                  (ILApp (ILRef 'kernel '+) '()))
    (check-ilexpr (App (kident '/) '())
                  '()
                  (ILApp (ILRef 'kernel '/) '()))
    (check-ilexpr (App (kident '+) (list (Quote 1) (Quote 2)))
                  '()
                  (ILBinaryOp '+ (list (ILValue 1) (ILValue 2))))
    (check-ilexpr (App (kident '-) (list (Quote 1)
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
       (Lambda '(a b) (list (App (kident 'add)
                                           (LI* 'a 'b)))
                    #t)
       (Lambda '(a b c) (list (App (kident 'mul)
                                             (LI* 'a 'b 'c)))
                    #t)))
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
     (App absyn-js-ffi (list (Quote 'var) (Quote 'console)))
     '()
     'console)

    (check-ilexpr
     (App
      absyn-js-ffi
      (list (Quote 'ref) (Quote 'obj) (Quote 'key1) (Quote 'key2)))
     '()
     (ILRef (ILRef (~sym 'obj) 'key1) 'key2))

    (check-ilexpr
     (App
      absyn-js-ffi
      (list (Quote 'ref)
            (LocalIdent 'obj) (Quote 'key1) (Quote 'key2)))
     '()
     (ILRef (ILRef 'obj 'key1) 'key2))

    (check-ilexpr
     (App
      absyn-js-ffi
      (list (Quote 'index)
            (Quote 'obj)
            (App (LocalIdent 'do) (list (Quote "Hello!")))))
     '()
     (ILIndex (~sym 'obj) (ILApp 'do (list (~str "Hello!")))))

    (check-ilexpr
     (App
      absyn-js-ffi
      (list (Quote 'object)
            (Quote 'key1) (Quote 'key2) (Quote 'key3)
            (Quote 'val1) (Quote 'val2) (Quote 'val3)))
     '()
     (ILObject (list (cons 'key1 (~sym 'val1))
                     (cons 'key2 (~sym 'val2))
                     (cons 'key3 (~sym 'val3)))))

    (check-ilexpr
     (App
      absyn-js-ffi
      (list (Quote 'array)
            (Quote 1) (Quote 2) (Quote 3)))
     '()
     (ILArray (list (ILValue 1) (ILValue 2) (ILValue 3))))

    (check-ilexpr
     (App
      absyn-js-ffi
      (list (Quote 'assign)
            (LocalIdent 'name) (Quote "John Doe")))
     (list (ILAssign 'name (~str "John Doe")))
     (ILValue (void))) ;;TODO: FFI special case!

    (check-ilexpr
     (App
      absyn-js-ffi
      (list (Quote 'new) (LocalIdent 'Array)))
     '()
     (ILNew 'Array))

    (check-ilexpr
     (App
      absyn-js-ffi
      (list (Quote 'throw) (Quote "What")))
     (list (ILThrow (~str "What")))
     (ILValue (void)))

    (check-ilexpr
     (App
      absyn-js-ffi
      (list (Quote 'operator) (Quote '+) (Quote 1) (Quote 2)))
     '()
     (ILBinaryOp '+ (list (ILValue 1) (ILValue 2)))))

  ;; --------------------------------------------------------------------------

  (test-case "Top Level Forms"
    (check-iltoplevel
     (list (App (kident 'display) (list (Quote "hello")))
           (App (kident 'print) (list (Quote "what" )
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
       (App (kident 'values)
                 (list (Quote 42)
                       (Lambda '(x) (LI* 'x) #t))))
     (list
      (ILVarDec
       'let_result1
       (ILApp (ILRef 'kernel 'values)
              (list (ILValue 42) (ILLambda '(x) (list (ILReturn 'x))))))
      (ILVarDec 'x (ILApp (ILRef 'let_result1 'getAt) (list (ILValue 0))))
      (ILVarDec 'ident (ILApp (ILRef 'let_result1 'getAt) (list (ILValue 1)))))))))
