#lang racketscript/boot

(require racketscript/interop
         racket/stxparam
         (for-syntax racket/list
                     racket/format
                     syntax/parse))

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
         check/pair-of?
         define-checked
         define-checked+provide)

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
      [(_ i:expr) #'($ 'arguments i)]
      [(_ i:expr j:expr) #'($ 'arguments i)]
      [arguments #'($ 'arguments)]))
  (syntax-parse stx
    [(_ args:id body ...+)
     #`(syntax-parameterize ([arguments #,-arguments])
         (λ ()
           (define args (#js.Core.argumentsToArray arguments))
           body ...))]
    [(_ (a0:id ...) body ...+)
     #`(syntax-parameterize ([arguments #,-arguments])
         (λ (a0 ...)
           body ...))]
    [(_ (a0:id ...+ . as:id) body ...+)
     (define fixed-args (length (syntax-e #'(a0 ...))))
     #`(syntax-parameterize ([arguments #,-arguments])
         (λ (a0 ...)
           (define as (#js.Array.prototype.slice.call arguments #,fixed-args))
           body ...))]))

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

(define default-check-message "Given: {0}, Expected: {1}, At: {2}")

(define-syntax-rule (type-check/raise type what)
  (unless (#js.type.check what)
    (throw (#js.Core.racketContractError "expected a {0}, but given {1}"
                                         type
                                         what))))

(define-syntax (check/raise stx)
  (syntax-parse stx
    [(_ (~datum #t) what at)
     #`(begin)]
    [(_ chkfn:id what at)
     #`(check/raise chkfn what #,(symbol->string (syntax-e #'chkfn)) at)]
    [(_ chkfn what at)
     #`(check/raise chkfn what #,(~a (syntax->datum #'chkfn)) at)]
    [(_ chkfn what expected at)
     #'(unless (chkfn what)
         (throw
          (#js.Core.racketContractError default-check-message
                                        expected
                                        what
                                        at)))]))

(define-syntax-rule (check/or c1 ...)
  (λ (v)
    (and (c1 v) ...)))

(define-syntax-rule (check/and c1 ...)
  (λ (v)
    (and (c1 v) ...)))

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
  (syntax-parse stx
    [(_ (name:id [arg:id checkfn] ...) body ...)
     #:with (n ...) #`#,(range (length (syntax-e #'(arg ...))))
     #`(define (name arg ...)
         (check/raise checkfn arg n) ...
         body ...)]))

(define-syntax (define-checked+provide stx)
  (syntax-parse stx
    [(_ (name:id e ...) body ...)
     #`(begin (define-checked (name e ...) body ...)
              (provide name))]))
