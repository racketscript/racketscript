#lang racketscript/boot

(require (for-syntax racket/base
                     racket/format
                     racket/list
                     syntax/parse)
         racket/stxparam
         racketscript/interop)

(provide throw
         new
         array
         object
         :=
         binop
         typeof
         instanceof
         *null*
         *undefined*
         *this*
         define-binop
         and
         or
         introduce-id
         define+provide
         arguments
         v-λ
         loop+
         for/array
         Math
         Number
         String
         Uint8Array
         Date
         Array
         RegExp
         console
         type-check/raise
         check/raise
         check/and
         check/or
         check/not
         check/pair-of?
         define-checked
         define-checked+provide
         define-nyi
         define-nyi+provide)

;; ----------------------------------------------------------------------------

(define-syntax introduce-id
  (syntax-parser
    [(_ id:id) #'(define-syntax id
                   (syntax-parser
                     [(_ e0 (... ...)) #'(($ 'id) e0 (... ...))]
                     [_:id #'($ 'id)]))]))

(define-syntax (define+provide stx)
  (syntax-parse stx
    [(_ name:id val:expr)
     #'(begin (provide name)
              (define name val))]
    [(_ (~and formals (name:id . args)) body ...)
     #'(begin (provide name)
              (define formals body ...))]))

;; ----------------------------------------------------------------------------
;; JS imports

(define+provide Kernel ($/require/* "./kernel.js")) ;; old stuff
(define+provide Core   ($/require/* "./core.js"))
(define+provide Paramz ($/require/* "./paramz.js"))
(define+provide Values #js.Core.Values)
(define+provide Pair   #js.Core.Pair)

;; Use some Native JS libs

(introduce-id Math)
(introduce-id Number)
(introduce-id String)
(introduce-id Uint8Array)
(introduce-id Date)
(introduce-id Array)
(introduce-id RegExp)
(introduce-id console)

;; ----------------------------------------------------------------------------
;; Interop Helpers

(define-syntax throw   (make-rename-transformer #'$/throw))
(define-syntax new     (make-rename-transformer #'$/new))
(define-syntax array   (make-rename-transformer #'$/array))
(define-syntax object  (make-rename-transformer #'$/obj))
(define-syntax :=      (make-rename-transformer #'$/:=))
(define-syntax binop   (make-rename-transformer #'$/binop))
(define-syntax typeof  (make-rename-transformer #'$/typeof))
(define-syntax instanceof  (make-rename-transformer #'$/instanceof))

(define-syntax *null*       (make-rename-transformer #'$/null))
(define-syntax *undefined*  (make-rename-transformer #'$/undefined))
(define-syntax *this*  (make-rename-transformer #'$/this))

(define-syntax define-binop
  (syntax-parser
    [(_ name:id oper:id)
     #`(define-syntax name
         (syntax-parser
           [(op e0:expr e1:expr) #`(binop oper e0 e1)]
           [(op e0:expr e1:expr en:expr ...+)
            #'(op (binop oper e0 e1) en (... ...))]))]))

(define-binop and &&)
(define-binop or \|\|)

(define-syntax-parameter arguments
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used in JS vararg lambda")))

;; v-λ is a lambda with JS semantics. More specifically, for rest
;; parameters are plain arrays instead of Racket lists. For sake of
;; performance, v-λ is preferred for writing variadic functions in
;; this file. `arguments` is syntax parameter to get the native JS
;; arguments object.
;;
;; NOTE: Use `arguments` directly with extreme care! Loops are
;; lambdas, therefore, rebind this variable with different name.
(define-syntax (v-λ stx)
  (define (-arguments stx)
    (syntax-parse stx
      [(_ i:expr) #'($ $/arguments i)]
      [(_ i:expr j:expr) #'($ $/arguments i)]
      [arguments #'$/arguments]))

  (define-values (stx* unchecked?)
    (syntax-parse stx
      [(v-λ args #:unchecked body ...+) (values #'(v-λ args body ...) #t)]
      [v (values stx #f)]))

  ;; TODO: raise-arity-error instead of contract error
  #`(syntax-parameterize ([arguments #,-arguments])
      #,(make-unchecked-lambda-syntax
         (syntax-parse stx*
           [(_ args:id body ...+) ;; Always unchecked
            #`(#%plain-lambda ()
                (define args (#js.Core.argumentsToArray arguments))
                body ...)]
           [(_ (a0:id ...) body ...+)
            #`(#%plain-lambda (a0 ...)
                #,(unless unchecked?
                    #`(when (binop !== #js.arguments.length #,(length (syntax-e #'(a0 ...))))
                        (throw (#js.Core.racketContractError "arity mismatch"))))
                body ...)]
           [(_ (a0:id ...+ . as:id) body ...+)
            (define fixed-args (length (syntax-e #'(a0 ...))))
            #`(#%plain-lambda (a0 ...)
                #,(unless unchecked?
                    #`(when (binop < #js.arguments.length #,fixed-args)
                        (throw (#js.Core.racketContractError "arity mismatch"))))
                (define as (#js.Array.prototype.slice.call arguments #,fixed-args))
                body ...)]))))

(begin-for-syntax
  (define (make-unchecked-lambda-syntax stx)
    (syntax-property stx 'racketscript-unchecked-lambda? #t)))

(define-syntax (loop+ stx)
  (syntax-parse stx
    [(_ [index:id start:expr end:expr step:expr] body ...+)
     #`(let loop ([index start])
         (when (binop < index end)
           body ...
           (loop (binop + index step))))]
    [(_ [index:id start:expr end:expr] body ...+)
     #'(loop+ [index start end 1] body ...)]
    [(_ [index:id end:expr] body ...+)
     #'(loop+ [index 0 end 1] body ...)]))

(define-syntax (for/array stx)
  (syntax-parse stx
    [(_ [(index:id item:id) arr:expr start:expr end:expr] body ...+)
     ;; TODO: save the arr than than copying it everywhere
     #'(loop+ [index start end 1]
         (define item ($ arr index))
          body ...)]
    [(_ [(index:id item:id) arr:expr] body ...+)
     #'(for/array [(index item) arr 0 ($ arr 'length)] body ...)]
    [(_ [item:id arr:expr start:expr end:expr] body ...+)
     #'(for/array [(i item) arr start end] body ...)]
    [(_ [item:id arr:expr start:expr] body ...+)
     #'(for/array [item arr start ($ arr 'length)] body ...)]
    [(_ [item:id arr:expr] body ...+)
     #'(for/array [item arr 0 ($ arr 'length)] body ...)]))


;; ----------------------------------------------------------------------------
;; Errors

(define-syntax-rule (type-check/raise type what)
  (unless (#js.type.check what)
    (throw (#js.Core.racketContractError "expected a" type
                                         ", but given" what))))

;; #:who arg allows replicating Racket error msg ala raise-argument-error
(define-syntax (check/raise stx)
  (syntax-parse stx
    [(_ #:who who (~datum #t) what at)
     #`(begin)]
    [(_ #:who who chkfn:id what at)
     #`(check/raise #:who who chkfn what #,(symbol->string (syntax-e #'chkfn)) at)]
    [(_ #:who who chkfn what at)
     #`(check/raise #:who who chkfn what #,(~a (syntax->datum #'chkfn)) at)]
    [(_ #:who who chkfn what expected at)
     #'(unless (chkfn what)
         (#js.Kernel.doraise
          (#js.Core.makeArgumentError who expected what)))]
    ;; no #:who arg cases, keep for backwards compat for now, TODO: remove?
    [(_ (~datum #t) what at)
     #`(begin)]
    [(_ chkfn:id what at)
     #`(check/raise chkfn what #,(symbol->string (syntax-e #'chkfn)) at)]
    [(_ chkfn what at)
     #`(check/raise chkfn what #,(~a (syntax->datum #'chkfn)) at)]
    [(_ chkfn what expected at)
     #'(unless (chkfn what)
         (throw
          (#js.Core.racketContractError "Expected:" expected ", given:" what
                                        ", at:" at)))]))


(define-syntax-rule (check/or c1 ...)
  (λ (v)
    (or (c1 v) ...)))

(define-syntax-rule (check/and c1 ...)
  (λ (v)
    (and (c1 v) ...)))

(define-syntax-rule (check/not c)
  (λ (v)
    (not (c v))))

(define-syntax (check/pair-of? stx)
  (syntax-parse stx
    [(_ (~datum #t) c2)
     #'(λ (v)
         (and (#js.Core.Pair.check v)
              (c2 #js.v.tl)))]
    [(_ c1 (~datum #t))
     #'(λ (v)
         (and (#js.Core.Pair.check v)
              (c1 #js.v.hd)))]
    [(_ c1 c2)
     #'(λ (v)
         (and (#js.Core.Pair.check v)
              (c1 #js.v.hd)
              (c1 #js.v.tl)))]))

(define-syntax (define-checked stx)
  (define-syntax-class checked-arg
    (pattern name:id)
    (pattern [name:id checkfn]))

  (syntax-parse stx
    [(_ (name:id arg:checked-arg  ...) body ...)
     #:with (n ...) #`#,(range (length (syntax-e #'(arg ...))))
     #:with ([n* (c-arg-name:id c-arg-checkfn)] ...)
     (filter (syntax-parser
               [(n [name:id checkfn]) #t]
               [(n name:id) #f])
             (syntax->list #'((n arg) ...)))
     #`(define name
         (v-λ (arg.name ...) #:unchecked
           (check/raise #:who 'name c-arg-checkfn c-arg-name n*) ...
           body ...))]))

(define-syntax (define-checked+provide stx)
  (syntax-parse stx
    [(_ (name:id e ...) body ...)
     #`(begin (define-checked (name e ...) body ...)
              (provide name))]))


(define-syntax (define-nyi stx)
  (syntax-parse stx
    [(_ n:id)
     #'(define name (lambda _ (throw (#js.Core.racketCoreError 'name " is not yet implemented"))))]))


(define-syntax-rule (define-nyi+provide id)
  (begin (define-nyi id) (provide id)))
