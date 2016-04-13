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
         "environment.rkt"
         "util.rkt"
         "absyn.rkt"
         "il.rkt")

(provide assemble
         assemble-module
         assemble-statement*
         assemble-statement)

(: assemble (-> ILProgram Void))
(define (assemble p)
  (assemble-statement* p (current-output-port)))

(: assemble-expr (-> ILExpr Output-Port Void))
(define (assemble-expr expr out)
  (define emit (curry fprintf out))
  (: emit-args (-> (Listof ILExpr) String Void))
  (define (emit-args args sep)
    (let loop ([a* args])
      (match a*
        ['() (void)]
        [(cons a '()) (assemble-expr a out)]
        [(cons a tl)
         (assemble-expr a out)
         (emit sep)
         (loop tl)])))
  
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
     (emit-args args ",")
     (emit ")")]
    [(ILBinaryOp oper args)
     (emit-args args (~a oper))]
    [(ILValue v) (assemble-value v out)]
    [(ILField expr fieldname)
     (assemble-expr expr out)
     (emit (~a "." (normalize-symbol fieldname)))] ;; TODO: or assmeble-expr the symbol
    [(ILSubscript expr fieldname)
     (assemble-expr expr out)
     (emit "[")
     (if (symbol? fieldname)
         (assemble-value (~a fieldname) out)
         (assemble-value fieldname out))
     (emit "]")]
    [(ILArray items)
     (emit "[")
     (emit-args items ",")
     (emit "]")]
    [(ILObject items)
     (emit "{")
     (for/last? ([i last? items])
                (emit (format "'~a':" (car i)))
                (assemble-expr (cdr i) out)
                (unless last?
                  (emit ",")))]
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
  (let ([cb (λ ([out : Output-Port])
              (assemble-requires* requires out)
              (for ([b body])
                (assemble-statement b out))
              (assemble-provides* provides out))])
    (if (print-to-stdout)
        (cb (current-output-port))
        (call-with-output-file (module-output-file id) #:exists 'replace cb))))

(: assemble-requires* (-> (Listof ILRequire) Output-Port Void))
(define (assemble-requires* reqs* out)
  (define emit (curry fprintf out))
  (emit (format "import * as ~a from '~a';"
                (jsruntime-core-module)
                (jsruntime-import-path (assert (current-source-file) path?)
                                       (jsruntime-core-module-path))))
  (for ([req reqs*])
    (match-define (ILRequire name idents) req)
    (emit (~a "import "
              "{" (string-join (map normalize-symbol idents) ", ") "} "
              "from '" name "';"))))

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
    [(symbol? v) (emit (~a (name-in-module 'core 'Symbol.make) "('" v "')"))]
    [(string? v) (write v out)]
    [(number? v) (emit (~a v))]
    [(boolean? v) (emit (if v "true" "false"))]
    [(empty? v) (emit (~a (name-in-module 'core 'Empty)))]
    [(list? v)
     (emit (~a (name-in-module 'core 'makeList) "("))
     (for/last? ([item last? v])
                (match item
                  [(Quote v) (assemble-value v out)]
                  [_ (assemble-value item out)])
                (unless last?
                  (emit ", ")))
     (emit ")")]
    [(cons? v)
     (emit (~a (name-in-module 'core 'Pair.make) "("))
     (assemble-value (car v) out)
     (emit ", ")
     (assemble-value (cdr v) out)
     (emit ")")]
    [else (displayln v) (error "TODO: Check how this thing actually works!")]))
