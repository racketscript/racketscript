#lang typed/racket/base

;;; Generate JavaScript code from abstract syntax. Each binding name
;;; in assumed to be fresh, to enforce lexical scope rules of Racket
;;; in JavaScript

(require racket/string
         racket/format
         racket/match
         racket/function
         "config.rkt"
         "absyn.rkt"
         "il.rkt")

(provide assemble
         assemble-module
         assemble-statement*
         assemble-statement)

(: normalize-symbol (-> Symbol String))
(define (normalize-symbol s)
  ;; TODO: handle every weird character in symbol
  ;; Since every identifier is suffixed with fresh symbol
  ;; we don't have to worry about name clashes after this
  ;; naive renaming
  (regexp-replace* #rx"[^a-zA-Z0-9_]*" (~a s) ""))

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
     (emit (~a (normalize-symbol vref) "[" index "]"))
     (emit ";")]
    [_ #:when (ILModule? stmt) (assemble-module stmt out)]
    [_ #:when (ILExpr? stmt)
       (assemble-expr stmt out)
       (emit ";")]))

(: assemble-module (-> ILModule Output-Port Void))
(define (assemble-module mod out)
  (define emit (curry fprintf out))
  (match-define (ILModule id body) mod)
  (emit "function() {")

  (for ([b body])
    (assemble-statement b out))
  
  (emit "}();"))

(: assemble-value (-> Any Output-Port Void))
(define (assemble-value d out)
  (define emit (curry fprintf out))
  ;; TODO: this will eventually be replaced by runtime primitives
  (cond
    [(symbol? d) (emit (~a "\"" d "\""))]
    [(string? d) (emit (~a "\"" d "\""))]
    [(number? d) (emit (~a d))]))
