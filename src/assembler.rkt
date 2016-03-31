#lang typed/racket/base

;;; Generate JavaScript code from abstract syntax. Each binding name
;;; in assumed to be fresh, to enforce lexical scope rules of Racket
;;; in JavaScript

(require racket/string
         racket/format
         racket/match
         racket/function
         "config.rkt"
         "util.rkt"
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
  (: char-map (HashTable String String))
  (define char-map
    #hash(("-" . "_")
          ("?" . "_p")
          ("+" . "_plus_")
          ("'" . "_prime_")
          ("*" . "_star_")
          ("/" . "_by_")
          ("=" . "_eq_")
          ("<" . "_lt_")
          (">" . "_gt_")
          ("!" . "_bang_")
          ("." . "_dot_")
          ("&" . "_and_")))
  (define char-list (string->list (symbol->string s)))
  (string-join
   (map (λ ([ch : Char])
          (define sch (string ch))
          (cond
            [(or (char-numeric? ch) (char-alphabetic? ch))
             sch]
            [(hash-has-key? char-map sch)
             (hash-ref char-map sch)]
            [else "_"]))
        char-list)
   ""))

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
  (match-define (ILModule id provides requires body) mod)

  (assemble-requires* requires out)
  (for ([b body])
    (assemble-statement b out))
  (assemble-provides* provides out))

(: assemble-requires* (-> (Listof ILRequire) Output-Port Void))
(define (assemble-requires* r* out)
  (void)) ;;; TODO

(: assemble-provides* (-> (Listof ILProvide) Output-Port Void))
(define (assemble-provides* p* out)
  (define emit (curry fprintf out))

  (emit "export { ")
  (for/last? ([p last? p*])
     (emit (~a (ILProvide-id p)))
     (unless last?
       (emit ",")))

  (emit " };"))

(: assemble-value (-> Any Output-Port Void))
(define (assemble-value d out)
  (define emit (curry fprintf out))
  ;; TODO: this will eventually be replaced by runtime primitives
  (cond
    [(symbol? d) (emit (~a "\"" d "\""))]
    [(string? d) (emit (~a "\"" d "\""))]
    [(number? d) (emit (~a d))]
    [(boolean? d) (emit (if d "true" "false"))]
    [(list? d)
     (emit "__$RACKETJS.primitives.makeList(")
     (for/last? ([item last? d])
                (match item
                  [(Quote v) (assemble-value v out)]
                  [_ (assemble-value item out)])
                (unless last?
                  (emit ", ")))
     (emit ")")]))
