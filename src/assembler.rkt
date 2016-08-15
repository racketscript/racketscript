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
    [(ILRef e s)
     (cond
       [(symbol? e) (emit (normalize-symbol e))]
       [(ILRef? e)
        (assemble-expr e out)]
       [else
        (emit "(")
        (assemble-expr e out)
        (emit ")")])
     (emit (~a "." (normalize-symbol s)))]
    [(ILIndex e e0)
     (if (symbol? e)
         (emit (normalize-symbol e))
         (assemble-expr e out))
     (emit "[")
     (assemble-expr e0 out)
     (emit "]")]
    [(ILNew v)
     (emit "new ")
     (assemble-expr v out)]
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
     (emit "} else {")
     (assemble-statement* f-branch out)
     (emit "}")]
    [(ILAssign lv rv)
     (assemble-expr lv out)
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
  (emit (format "import * as ~a from '~a';"
                (jsruntime-kernel-module)
                (jsruntime-import-path (assert (current-source-file) path?)
                                       (jsruntime-kernel-module-path))))
  (for ([req reqs*])
    (match-define (ILRequire mod obj-name) req)
    (emit (format "import * as ~a from \"~a\";"
                  obj-name
                  mod))))

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
    [(Quote? v) (assemble-value (Quote-datum v) out)] ;; FIXME
    [(symbol? v) (emit (~a (name-in-module 'core 'Symbol.make) "('" v "')"))]
    [(keyword? v) (emit (~a (name-in-module 'core 'Keyword.make) "('" v "')"))]
    [(string? v) (write v out)]
    [(number? v)
     (match v
       [+inf.0 (emit "Infinity")]
       [-inf.0 (emit "-Infinity")]
       [+nan.0 (emit "NaN")]
       [+nan.f  (emit "NaN")]
       [_ (emit (~a v))])] ;; TODO
    [(boolean? v) (emit (if v "true" "false"))]
    [(empty? v) (emit (~a (name-in-module 'core 'Pair.Empty)))]
    [(list? v)
     (emit (~a (name-in-module 'core 'Pair.makeList) "("))
     (for/last? ([item last? v])
                (match item
                  [(Quote v) (assemble-value v out)]
                  [_ (assemble-value item out)])
                (unless last?
                  (emit ", ")))
     (emit ")")]
    [(vector? v)
     (emit (~a (name-in-module 'core 'Vector.make) "(["))
     (for/last? ([item last? (vector->list (cast v (Vectorof Any)))]) ;; HACK
                (match item
                  [(Quote v) (assemble-value v out)]
                  [_ (assemble-value item out)])
                (unless last?
                  (emit ", ")))
     (emit "], true)")]
    [(hash? v)
     (emit "{}")]
    [(cons? v)
     (emit (~a (name-in-module 'core 'Pair.make) "("))
     (assemble-value (car v) out)
     (emit ", ")
     (assemble-value (cdr v) out)
     (emit ")")]
    [(char? v)
     (write (~a v) out)]
    [(bytes? v)
     (define byte-vals
       (string-join (map number->string (bytes->list v)) ","))
     (emit (format "new Uint8Array([~a])" byte-vals))]
    [(regexp? v)
     (define s (string-replace (cast (object-name v) String) "/" "\\/"))
     (write (format "/~a/" s) out)]
    [(byte-regexp? v)
     (define s (string-replace (bytes->string/utf-8 (cast (object-name v) Bytes)) "/" "\\/"))
     (write (format "/~a/" s) out)]
    [(void? v)
     (emit "null")]
    [else (displayln v) (error "TODO: Check how this thing actually works!")]))

[module+ test
  (require typed/rackunit
           racket/port)

  ;; TODO: Replace with this, but fails to typecheck.
  #;(define-syntax-rule (define/check name fn)
    (define-binary-check (name il-actual expected-out)
      (let ([out-port (open-output-string)])
       (equal? (begin (fn il-actual out-port)
                       (get-output-string out-port))
                expected-out))))

  (define-syntax-rule (check-asm fn il out-str msg)
    (let ([out-port (open-output-string)])
      (check-equal? (begin (fn il out-port)
                           (get-output-string out-port))
                    out-str
                    msg)))

  (define-syntax-rule (define/check name fn)
    (define-syntax name
      (syntax-rules ()
        [(_ il out-str) (check-asm fn il out-str "")]
        [(_ il out-str msg) (check-asm fn il out-str msg)])))

  (define/check check-expr      assemble-expr)
  (define/check check-stm       assemble-statement)
  (define/check check-value     assemble-value)
  (define/check check-requires  assemble-requires*)

  ;;; Values ------------------------------------------------------------------

  ;; Numbers
  (check-value 12 "12")
  
  ;; Strings
  (check-value "Hello World!" "\"Hello World!\"")
  (check-value 'hello (format "~a('hello')" (name-in-module 'core 'Symbol.make)))

  ;; Booleans
  (check-value #t "true")
  (check-value #f "false")

  ;; Lists and pairs
  (check-value '() (~a (name-in-module 'core 'Pair.Empty)))
  (check-value '(1) (~a (name-in-module 'core 'Pair.makeList) "(1)"))
  (check-value '(1 2) (~a (name-in-module 'core 'Pair.makeList) "(1, 2)"))
  (check-value '(1 2 (3 4) 5) (format "~a(1, 2, ~a(3, 4), 5)"
                                      (name-in-module 'core 'Pair.makeList)
                                      (name-in-module 'core 'Pair.makeList)))
  (check-value '(1 . 2) (format "~a(1, 2)" (name-in-module 'core 'Pair.make)))
  (check-value '(1 2 . 3) (format "~a(1, ~a(2, 3))"
                                  (name-in-module 'core 'Pair.make)
                                  (name-in-module 'core 'Pair.make)))

  (check-value (void) "null")

  ;; Vectors
  (check-value #(1 2 3) (format "~a([1, 2, 3], true)"
                                (name-in-module 'core 'Vector.make))
               "immutable vector")
  (check-value #(1 2 3 (4 5)) (format "~a([1, 2, 3, ~a(4, 5)], true)"
                                       (name-in-module 'core 'Vector.make)
                                       (name-in-module 'core 'Pair.makeList))
               "immutable vector with nested list")

  ;;; Expressions -------------------------------------------------------------

  ;; Values most should be covered above
  (check-expr (ILValue "Hello World!") "\"Hello World!\"")
  (check-expr (ILValue 12) "12")

  ;; Lambda
  (check-expr (ILLambda '(x) (list 'x))
              "function(x) {x;}")
  (check-expr (ILLambda '(x) (list (ILReturn 'x)))
              "function(x) {return x;}")
  (check-expr (ILLambda '(a b c) (list (ILReturn (ILBinaryOp '+ '(a b c)))))
              "function(a, b, c) {return a+b+c;}")

  ;; Application
  (check-expr (ILApp 'add (list 'a 'b)) "add(a,b)")
  (check-expr (ILApp 'add (list (ILValue "foo") (ILValue "bar"))) "add(\"foo\",\"bar\")")
  (check-expr (ILApp (ILLambda '(x) (list (ILReturn 'x))) (list (ILValue "Hello")))
              "(function(x) {return x;})(\"Hello\")")

  ;; Rest
  (check-expr 'foobar "foobar"  "assemble an identifier")
  (check-expr (ILIndex 'arr 'i) "arr[i]" "object indexing")
  (check-expr (ILIndex 'arr (ILBinaryOp '+ (list 'i (ILValue 1))))
              "arr[i+1]"
              "object indexing with expression")
  (check-expr (ILRef 'document 'write)
              "document.write"
              "object field ref")
  (check-expr (ILRef (ILNew (ILApp 'Array (list (ILValue 10) (ILValue 0)))) 'property)
              "(new Array(10,0)).property"
              "create object via expression and get a field with expression in paren")
  (check-expr (ILRef (ILRef (ILRef 'global 'window) 'document) 'write)
              "global.window.document.write"
              "successive refs to object shouldn't generate parens")

  ;; NOTE: These are some cases whose output could be improved in future by reducing
  ;; unnecessary parens
  (check-expr (ILRef (ILApp (ILRef 'Array 'sort) '(a lt)) 'size)
              "((Array.sort)(a,lt)).size")
  (check-expr (ILRef (ILApp 'sort '(a lt)) 'size)
              "(sort(a,lt)).size")


  ;;; Statements --------------------------------------------------------------

  ;; declaration
  (check-stm (ILVarDec 'sum (ILBinaryOp '+ (list (ILValue 2) (ILValue 4))))
             "var sum = 2+4;")
  (check-stm (ILVarDec 'sum (ILLambda '(a b) (list (ILReturn (ILBinaryOp '+ '(a b))))))
             "var sum = function(a, b) {return a+b;};")
  (check-stm (ILAssign 'sum (ILBinaryOp '+ '(a b))) "sum = a+b;")
  (check-stm (ILIf (ILBinaryOp '< '(a b)) (list (ILValue #t)) (list (ILValue #f)))
             "if (a<b) {true;} else {false;}")

  ;;; Requires ----------------------------------------------------------------
  ;; TODO

  ;;; Provides ----------------------------------------------------------------
  ;; TODO
  
  ]
