#lang typed/racket/base

;;; Generate JavaScript code from abstract syntax. Each binding name
;;; in assumed to be fresh, to enforce lexical scope rules of Racket
;;; in JavaScript

(require racket/string
         racket/format
         racket/match
         racket/list
         racket/function
         "config.rkt"
         "util.rkt"
         "absyn.rkt"
         "il.rkt")

(provide assemble
         assemble-module
         assemble-statement*
         assemble-statement)

(require/typed "config.rkt"
  [module-output-file (-> (U String Symbol) Path)]
  [output-directory (Parameter String)])

(: assemble (-> ILProgram Void))
(define (assemble p)
  (assemble-statement* p (current-output-port)))

(: assemble-expr (-> ILExpr Output-Port Void))
(define (assemble-expr expr out)
  (define emit (curry fprintf out))
  (match expr
    [(ILLambda args exprs)
     (emit "function(")
     (emit (string-join (map normalize-symbol args) ", "))
     (emit ") {")
     (for ([e exprs])
       (assemble-statement e out))
     (emit "}")]
    [(ILApp lam args)
     (unless (symbol? lam) (emit "("))
     (assemble-expr lam out)
     (unless (symbol? lam) (emit ")"))
     (emit "(")
     (let loop ([a* args])
       (match a*
         ['() (void)]
         [(cons a '()) (assemble-expr a out)]
         [(cons a tl)
          (assemble-expr a out)
          (emit ",")
          (loop tl)]))
     (emit ")")]
    [(ILBinaryOp oper right left) (void)]
    [(ILValue v) (assemble-value v out)]
    [_ #:when (symbol? expr) (emit (~a (normalize-symbol expr)))]
    [_ (error "unsupported expr" (void))]))

(: assemble-statement* (-> ILStatement* Output-Port Void))
(define (assemble-statement* stmt* out)
  (for [(s stmt*)]
    (assemble-statement s out)))

(: assemble-statement (-> ILStatement Output-Port Void))
(define (assemble-statement stmt out)
  (define emit (curry fprintf out))
  (match stmt
    [(ILVarDec id expr)
     (emit (~a "var " (normalize-symbol id)))
     (when expr
       (emit " = ")
       (assemble-expr expr out))
     (emit ";")]
    [(ILReturn expr)
     (emit "return ")
     (assemble-expr expr out)
     (emit ";")]
    [(ILIf expr t-branch f-branch)
     (emit "if (")
     (assemble-expr expr out)
     (emit ") {")
     (assemble-statement* t-branch out)
     (emit "} else { ")
     (assemble-statement* f-branch out)
     (emit "}")]
    [(ILAssign id rv)
     (emit (~a (normalize-symbol id)))
     (emit " = ")
     (assemble-expr rv out)
     (emit ";")]
    [(ILValuesMatch id vref index)
     (emit (~a "var " (normalize-symbol id)))
     (emit " = ")
     (emit (~a (normalize-symbol vref) ".getAt(" index ")"))
     (emit ";")]
    [_ #:when (ILModule? stmt) (assemble-module stmt out)]
    [_ #:when (ILExpr? stmt)
       (assemble-expr stmt out)
       (emit ";")]))

(: assemble-module (-> ILModule Output-Port Void))
(define (assemble-module mod out)
  (define emit (curry fprintf out))
  (match-define (ILModule id provides requires body) mod)
  (printf "[assemble] ~a\n" id)
  (call-with-output-file (module-output-file id) #:exists 'replace
    (λ ([out : Output-Port])
      (assemble-requires* requires out)
      (for ([b body])
        (assemble-statement b out))
      (assemble-provides* provides out))))

(: assemble-requires* (-> (Listof ILRequire) Output-Port Void))
(define (assemble-requires* r* out)
  (define emit (curry fprintf out))
  (emit "import * as __$RACKETCORE from 'core.js';")
  (emit "import * as __$RACKETKERNEL from 'kernel.js';")) ;;; TODO

(: assemble-provides* (-> (Listof ILProvide) Output-Port Void))
(define (assemble-provides* p* out)
  (define emit (curry fprintf out))

  (unless (empty? p*)
    (emit "export { ")
    (for/last? ([p last? p*])
               (emit (~a (normalize-symbol (ILProvide-id p))))
               (unless last?
                 (emit ",")))

    (emit " };")))

(: assemble-value (-> Any Output-Port Void))
(define (assemble-value v out)
  (define emit (curry fprintf out))
  ;; TODO: this will eventually be replaced by runtime primitives
  
  (cond
    [(Quote? v) (assemble-value (Quote-datum v) out)]
    [(symbol? v) (emit (~a "__$RACKETCORE.Symbol.make('" v "')"))]
    [(string? v) (emit (~a "\"" v "\""))]
    [(number? v) (emit (~a v))]
    [(boolean? v) (emit (if v "true" "false"))]
    [(empty? v) (emit "__$RACKETCORE.Empty")]
    [(list? v)
     (emit "__$RACKETCORE.makeList(")
     (for/last? ([item last? v])
                (match item
                  [(Quote v) (assemble-value v out)]
                  [_ (assemble-value item out)])
                (unless last?
                  (emit ", ")))
     (emit ")")]
    [(cons? v)
     (emit "__$RACKETCORE.Pair.make(")
     (assemble-value (car v) out)
     (emit ", ")
     (assemble-value (cdr v) out)
     (emit ")")]
    [else (displayln v) (error "TODO: Check how this thing actually works!")]))
