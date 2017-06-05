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
         "logging.rkt"
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

  (: wrap-e? (-> ILExpr Boolean))
  ;; Wrap expression 'e' when being applied, referenced
  ;; or indexed
  (define (wrap-e? e)
    (or (ILNew? e)
        (ILLambda? e)
        (ILBinaryOp? e)
        (ILObject? e)
        (and (ILValue? e)
             (number? (ILValue-v e))
             (byte? (ILValue-v e)))))

  (match expr
    [(ILLambda args body)
     (emit "function(")
     (emit (string-join (map normalize-symbol args) ", "))
     (emit ") {")
     (for ([s body])
       (assemble-statement s out))
     (emit "}")]
    [(ILApp lam args)
     (when (wrap-e? lam) (emit "("))
     (assemble-expr lam out)
     (when (wrap-e? lam) (emit ")"))
     (emit "(")
     (emit-args args ",")
     (emit ")")]
    [(ILBinaryOp oper args)
     (for/last? ([arg last? args])
       (when (ILBinaryOp? arg) (emit "("))
       (assemble-expr arg out)
       (when (ILBinaryOp? arg) (emit ")"))
       (unless last?
         (emit (~a oper))))]
    [(ILRef e s)
     (cond
       [(symbol? e) (emit (normalize-symbol e))]
       [(wrap-e? e)
        (emit "(")
        (assemble-expr e out)
        (emit ")")]
       [else
        (assemble-expr e out)])
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
                (if (string? (car i))
                    (begin (assemble-value (car i) out)
                           (emit ":"))
                    (emit (format "'~a':" (car i))))
                (assemble-expr (cdr i) out)
                (unless last?
                  (emit ",")))
     (emit "}")]
    [(ILInstanceOf expr type)
     ;;TODO: Remove parens
     (emit "(")
     (assemble-expr expr out)
     (emit ") instanceof (")
     (assemble-expr type out)
     (emit ")")]
    [(ILTypeOf expr)
     (emit "typeof(")
     (assemble-expr expr out)
     (emit ")")]
    [(ILValue v) (assemble-value v out)]
    [(ILNull)
     (emit "null")]
    [(ILUndefined)
     (emit "undefined")]
    [_ #:when (symbol? expr)
       (emit (~a (normalize-symbol expr)))]
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
    [(ILLetDec id expr)
     (emit (~a "let " (normalize-symbol id)))
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
    [(ILIf* clauses)
     (let loop ([clauses clauses])
       (match clauses
         [(cons (ILIfClause #f body) ctl)
          (emit "{")
          (assemble-statement* body out)
          (emit "}")]
         [(cons (ILIfClause (? ILExpr? pred) body) ctl)
          (emit "if (")
          (assemble-expr pred out)
          (emit ") {")
          (assemble-statement* body out)
          (emit "}")
          (when (cons? ctl)
            (emit " else ")
            (loop ctl))]))]
    [(ILWhile condition body)
     (emit "while (")
     (assemble-expr condition out)
     (emit "){")
     (for ([s body])
       (assemble-statement s out))
     (emit "}")]
    [(ILAssign lv rv)
     (assemble-expr lv out)
     (emit " = ")
     (assemble-expr rv out)
     (emit ";")]
    [(ILContinue lab)
     (emit "continue")
     (when lab
       (emit " ~a" (normalize-symbol lab)))
     (emit ";")]
    [(ILThrow expr)
     (emit "throw ")
     (assemble-expr expr out)
     (emit ";")]
    [(ILExnHandler try error catch finally)
     (emit "try {")
     (assemble-statement* try out)
     (emit "}")
     (when (cons? catch)
       (emit (~a " catch (" (normalize-symbol error) ") {"))
       (assemble-statement* catch out)
       (emit "}"))
     (when (cons? finally)
       (emit (~a " finally {"))
       (assemble-statement* finally out)
       (emit "}"))]
    [(ILLabel name)
     (emit "~a:" (normalize-symbol name))]
    [(ILValue v) #:when (void? v) (void)] ;; ignore this NOP case
    [_ #:when (ILExpr? stmt)
       ;; When we have a lambda expression as a statement, wrap it in
       ;; paranthesis. It's a JavaScript syntax ambiguity mentioned in
       ;; ECMAScript spec.
       (when (ILLambda? stmt)
         (emit  "("))
       (assemble-expr stmt out)
       (when (ILLambda? stmt)
         (emit ")"))
       (emit ";")]))

(: assemble-module (-> ILModule (Option Output-Port) Void))
(define (assemble-module mod maybeout)
  (match-define (ILModule id provides requires body) mod)
  (log-rjs-info "[assemble] ~a" id)
  (let ([cb (λ ([out : Output-Port])
              (assemble-requires* requires out)
              (for ([b body])
                (assemble-statement b out))
              (assemble-provides* provides out))])
    (if maybeout
        (cb maybeout)
        ;; For all other cases we need a valid module name, eg. kernel
        (call-with-output-file (module-output-file (assert id path?))
          #:exists 'replace
          cb))))

(: assemble-requires* (-> ILRequire* Output-Port Void))
(define (assemble-requires* reqs* out)
  (define emit (curry fprintf out))
  (define core-import-path
    ;; (current-source-file) should not be false, else abort
    (jsruntime-import-path (cast (current-source-file) (U Symbol Path))
                           (jsruntime-module-path 'core)))

  (emit (format "import * as ~a from '~a';"
                (jsruntime-core-module)
                core-import-path))
  (for ([req reqs*])
    (match-define (ILRequire mod obj-name import-sym) req)
    (define import-string
      (case import-sym
        [(default) (format "import ~a from \"~a\";"
                           (normalize-symbol obj-name)
                           mod)]
        [(*) (format "import * as ~a from \"~a\";"
                      (normalize-symbol obj-name)
                      mod)]
        [else (error 'assemble-requires* "invalid require mode")]))

    (emit import-string)))

(: assemble-provides* (-> ILProvide* Output-Port Void))
(define (assemble-provides* p* out)
  (define emit (curry fprintf out))

  (unless (empty? p*)
    (emit "export { ")
    (for/last? ([p last? p*])
               (match p
                 [(ILSimpleProvide id)
                  (emit (~a (normalize-symbol id)))]
                 [(ILRenamedProvide local-id exported-id)
                  (emit (format "~a as ~a"
                                (normalize-symbol local-id)
                                (normalize-symbol exported-id)))])
               (unless last?
                 (emit ",")))

    (emit " };")))

(: assemble-value (-> Any Output-Port Void))
(define (assemble-value v out)
  (define emit (curry fprintf out))
  ;; TODO: this will eventually be replaced by runtime primitives
  (cond
    [(Quote? v) (assemble-value (Quote-datum v) out)] ;; FIXME
    [(symbol? v)
     (emit (~a (name-in-module 'core 'Symbol.make) "("))
     (write (symbol->string v) out)
     (emit ")")]
    [(keyword? v) (emit (~a (name-in-module 'core 'Keyword.make) "('" v "')"))]
    [(string? v) (write v out)]
    [(number? v)
     (match v
       [+inf.0 (emit "Infinity")]
       [-inf.0 (emit "-Infinity")]
       [+nan.0 (emit "NaN")]
       [+nan.f  (emit "NaN")]
       [_ #:when (single-flonum? v) (emit (~a (exact->inexact (inexact->exact v))))]
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
     (: maker Symbol)
     (define maker (cond
                     [(hash-eq? v) 'Hash.makeEq]
                     [(hash-eqv? v) 'Hash.makeEqv]
                     [(hash-equal? v) 'Hash.makeEqual]
                     [else (error 'assemble-value "unknown hash type")]))
     (define mutable (not (immutable? v)))
     (emit (~a (name-in-module 'core maker) "(["))
     (for/last? ([key last? (hash-keys v)]
                 [val _ (hash-values v)])
       (emit "[")
       (assemble-value key out)
       (emit ",")
       (assemble-value val out)
       (emit "]")
       (unless last?
         (emit ", ")))
     (emit "], false)")]
    [(cons? v)
     (emit (~a (name-in-module 'core 'Pair.make) "("))
     (assemble-value (car v) out)
     (emit ", ")
     (assemble-value (cdr v) out)
     (emit ")")]
    [(box? v)
     (emit (~a (name-in-module 'core 'Box.make) "("))
     (assemble-value (unbox v) out)
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
     (define s (string-replace (bytes->string/utf-8
                                (cast (object-name v) Bytes))
                               "/" "\\/"))
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
  (check-value 'hello (format "~a(\"hello\")" (name-in-module 'core 'Symbol.make)))

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

  (check-stm (ILLambda '() '())
             "(function() {});"
             "Lambda as a lone statement should be wrapped in brackets")

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
  (check-expr (ILIndex (ILIndex 'arr (ILBinaryOp '+ (list 'i (ILValue 1))))
                       (ILBinaryOp '+ (list 'i 'j)))
              "arr[i+1][i+j]"
              "successive indexing")
  (check-expr (ILBinaryOp '+ '(a b))
              "a+b"
              "binary op")
  (check-expr (ILBinaryOp '\|\| '(a b))
              "a||b"
              "binary op")
  (check-expr (ILBinaryOp '\|\| (list (ILBinaryOp '&& '(a b))
                                    (ILBinaryOp '&& '(c d))))
              "(a&&b)||(c&&d)"
              "nested binary ops")

    ;;; ILRef -------------------------------------------------------------------

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
              "Array.sort(a,lt).size")
  (check-expr (ILRef (ILApp 'sort '(a lt)) 'size)
              "sort(a,lt).size")
  (check-expr (ILRef (ILLambda '(x) (list (ILReturn 'x))) 'valueOf)
              "(function(x) {return x;}).valueOf")
  (check-expr (ILRef (ILValue 42) 'valueOf)
              "(42).valueOf")
  (check-expr (ILRef (ILBinaryOp '+ '(a b)) 'valueOf)
              "(a+b).valueOf")
  (check-expr (ILRef (ILObject '()) 'valueOf)
              "({}).valueOf")
  (check-expr (ILRef (ILNew (ILApp 'String (list (ILValue "Hello!")))) 'valueOf)
              "(new String(\"Hello!\")).valueOf")

  ;; Objects

  (check-expr (ILObject (list (cons 'name (ILValue "Vishesh"))
                              (cons 'location (ILValue "Boston"))))
              "{'name':\"Vishesh\",'location':\"Boston\"}")

  (check-expr (ILObject (list (cons 'full-name (ILValue "Vishesh"))
                              (cons 'location (ILValue "Boston"))))
              "{'full-name':\"Vishesh\",'location':\"Boston\"}")

  ;; Arrays

  (check-expr (ILArray (list (ILValue 1) (ILValue "1") (ILObject '())))
              "[1,\"1\",{}]")

  ;; Instanceof
  (check-expr (ILInstanceOf (ILValue 1) (ILValue 2))
              "(1) instanceof (2)")

  ;;; Statements --------------------------------------------------------------

  ;; declaration
  (check-stm (ILVarDec 'sum (ILBinaryOp '+ (list (ILValue 2) (ILValue 4))))
             "var sum = 2+4;")
  (check-stm (ILVarDec 'sum (ILLambda '(a b) (list (ILReturn (ILBinaryOp '+ '(a b))))))
             "var sum = function(a, b) {return a+b;};")
  (check-stm (ILAssign 'sum (ILBinaryOp '+ '(a b))) "sum = a+b;")
  (check-stm (ILIf (ILBinaryOp '< '(a b)) (list (ILValue #t)) (list (ILValue #f)))
             "if (a<b) {true;} else {false;}")

  (check-stm (ILIf* (list
                     (ILIfClause (ILValue 1) (list 't-branch-1))
                     (ILIfClause (ILValue 2) (list 't-branch-2))
                     (ILIfClause (ILValue 3) (list 't-branch-3))
                     (ILIfClause #f (list 'done))))
             "if (1) {t_branch_1;} else if (2) {t_branch_2;} else if (3) {t_branch_3;} else {done;}")
  (check-stm (ILIf* (list
                     (ILIfClause (ILValue 1) (list 't-branch-1))))
             "if (1) {t_branch_1;}")

  ;; Exceptions

  (check-stm (ILThrow (ILValue 1))
             "throw 1;")

  (check-stm (ILThrow (ILNew 'Error))
             "throw new Error;")

  (check-stm (ILExnHandler (list (ILApp 'add (list (ILValue 1) (ILValue 2))))
                           'error
                           '()
                           (list (ILApp 'done '())))
             "try {add(1,2);} finally {done();}")

  (check-stm (ILExnHandler (list (ILApp 'add (list (ILValue 1) (ILValue 2))))
                           'error
                           (list (ILApp 'done '(error)))
                           '())
             "try {add(1,2);} catch (error) {done(error);}")

  (check-stm (ILExnHandler (list (ILApp 'add (list (ILValue 1) (ILValue 2))))
                           'error
                           (list (ILApp 'done '(error)))
                           (list (ILApp 'done '())))
             "try {add(1,2);} catch (error) {done(error);} finally {done();}")


  ;;; Requires ----------------------------------------------------------------
  ;; TODO

  ;;; Provides ----------------------------------------------------------------
  ;; TODO

  ]
