#lang racket/base

(provide #%js-ffi
         $
         $/new
         $/obj
         $/array
         $/require
         $/require/*
         $/require/rkt
         $$
         $>
         $/:=
         $/throw
         $/undefined
         $/null
         $/typeof
         $/instanceof
         $/arguments
         $/binop
         $/+
         $/str
         $/this
         =>$
         js-string
         js-string->string
         js-array->list
         assoc->object
         (rename-out [*in-js-array in-js-array]
                     [*in-js-object in-js-object])
         for/js-array
         js-array?
         for/js-object
         js-object?)

(require (for-syntax racket/base
                     racket/string
                     syntax/stx
                     threading)
         "private/interop.rkt"
         syntax/parse/define)

(begin-for-syntax
  (require (only-in "compiler/util-untyped.rkt" js-identifier?))
  (provide jsident js-identifier?)
  (define (ids->sym stx)
    (stx-map (λ (c) #`'#,c) stx))

  (define-syntax-class symbol
    #:description "match with symbol datum"
    (pattern ((~literal quote) var:id)))

  (define-syntax-class jsident
    #:description "JavaScript identifier"
    (pattern ((~literal quote) var:id)
             #:when (js-identifier? (syntax-e #'var))))

  (define (split-id id)
    (map string->symbol
         (string-split
          (symbol->string (syntax-e id)) "."))))

(define-syntax (-$ stx)
  (syntax-parse stx
    [(_ v:jsident)
     #`(#%js-ffi 'var v)]
    [(_ b:expr xs:symbol)
     #`(#%js-ffi 'ref b xs)]
    [(_ b:expr xs:expr)
     #`(#%js-ffi 'index b xs)]
    [_ (error '$ "no match")]))

(define-syntax ($ stx)
  (syntax-parse stx
    [(_ v:jsident) #'(-$ v)]
    [(_ b:expr xs:symbol) #'(-$ b xs)]
    [(_ b:expr xs:expr) #'(-$ b xs)]
    [(_ b:expr xs:expr xsr:expr ...+) #'($ (-$ b xs) xsr ...)]))

(define-syntax ($$ stx)
  (syntax-parse stx
    [(_ v:symbol e0:expr ...)
     #:with (vn) (stx-cdr #'v)
     #:with (id0 id1 ...) (split-id #'vn)
     #`(($ 'id0 'id1 ...) e0 ...)]
    [(_ v:id e0:expr ...)
     #:with (id0 id1 ...) (split-id #'v)
     #:with f-id (datum->syntax stx (syntax-e #'id0))
     #`(($ f-id 'id1 ...) e0 ...)]))

(define-syntax ($/new stx)
  (syntax-parse stx
    [(_ v:expr)
     #`(#%js-ffi 'new v)]))

(define-syntax ($/throw stx)
  (syntax-parse stx
    [(_ e:expr)
     #`(#%js-ffi 'throw e)]))

(define-syntax ($/undefined stx)
  (syntax-parse stx
    [_ #`(#%js-ffi 'undefined)]))

(define-syntax ($/null stx)
  (syntax-parse stx
    [_ #`(#%js-ffi 'null)]))

(define-syntax ($/this stx)
  (syntax-parse stx
    [_ #`(#%js-ffi 'this)]))

(define-syntax ($/arguments stx)
  (syntax-parse stx
    [_ #`(#%js-ffi 'arguments)]))

(define-syntax ($/obj stx)
  ;; TODO: What to do about ambiguity with the cases where fieldname
  ;; could be both string or symbol? Maybe generate string is it can't
  ;; be a symbol in JS?
  (syntax-parse stx
    [(_ [f v:expr] ...)
     #:with (new-f ...) (stx-map (λ (e)
                                   (cond
                                     [(identifier? e)
                                      (quasisyntax (quote #,(syntax-e e)))]
                                     [(string? (syntax-e e)) e]
                                     [else (error '$/obj "invalid key identifier")]))
                                 #'(f ...))
     #`(#%js-ffi 'object new-f ... v ...)]))

(define-syntax ($/:= stx)
  (syntax-parse stx
    ;;TODO: Check if e is a ref, index or symbol
    [(_ e v) #`(#%js-ffi 'assign e v)]))

(define-syntax ($/array stx)
  (syntax-parse stx
    [(_ e:expr ...)
     #`(#%js-ffi 'array e ...)]))

(define-syntax ($/require stx)
  (syntax-parse stx
    [(_ mod:str)
     #`(#%js-ffi 'require mod)]
    [(_ mod:str (~datum *))
     #`(#%js-ffi 'require '* mod)]))

(define-syntax ($/require/* stx)
  (syntax-parse stx
    [(_ mod:str)
     #`(#%js-ffi 'require '* mod)]))

(define-syntax ($/require/rkt stx)
  (syntax-parse stx
    [(_ mod:str)
     #`(#%js-ffi 'requirerkt mod)]
    [(_ mod:str (~datum *))
     #`(#%js-ffi 'requirerkt '* mod)]))

(define-syntax ($> stx)
  (define-syntax-class chaincall
    (pattern [fieldname:id ρ:expr ...]))

  (define-syntax-class part
    (pattern (~or cc:chaincall ci:id)))

  (syntax-parse stx
    [($> e:expr) #'e]
    [($> e:expr ci:id cc:part ...)
     #'($> ($ e 'ci) cc ...)]
    [($> e:expr cc0:chaincall cc:part ...)
     #'($> (($ e 'cc0.fieldname) cc0.ρ ...) cc ...)]))

(define (=>$ lam-expr)
  ;; FIXME: We are referring to the core module name directly!
  (($ ($ '$rjs_core) 'Marks 'wrapWithContext) lam-expr))

(define-syntax ($/typeof stx)
  (syntax-parse stx
    [(_ e:expr) #'(#%js-ffi 'typeof e)]
    [(_ e:expr (~and v:str
                     (~or (~datum "undefined")
                          (~datum "object")
                          (~datum "boolean")
                          (~datum "number")
                          (~datum "string")
                          (~datum "function"))))
     #'($/binop === (#%js-ffi 'typeof e) ($/str v))]))

(define-syntax ($/instanceof stx)
  (syntax-parse stx
    [(_ e:expr t:expr) #'(#%js-ffi 'instanceof e t)]))

(define-syntax ($/binop stx)
  (syntax-parse stx
    [(_ oper:id operand0:expr operand1:expr)
     #'(#%js-ffi 'operator 'oper operand0 operand1)]))

(define-syntax ($/+ stx)
  (syntax-parse stx
    [(_ e) #'e]
    [(_ e . rst) #'($/binop + e ($/+ . rst))]))

(define (js-string e)
  ($$ e.toString))

(define (js-string->string e)
  (($ ($ '$rjs_core) 'UString 'makeImmutable) e))

(define (js-array->list e)
  (($ ($ '$rjs_core) 'Pair 'listFromArray) e))

(define-syntax-parser $/str
  [(_ v:str) #'(#%js-ffi 'string v)]
  [(_ e:expr) #'(js-string e)])

(define (assoc->object pairs)
  (define result ($/obj))
  (let loop ([pairs pairs])
    (cond
      [(null? pairs) result]
      [else
       (define p (car pairs))
       (define key
         (let ([k (car p)])
           (cond
             [(or ($/typeof k "string") (string? k)) k] ; allow both js and racket string keys
             [(symbol? k) (symbol->string k)]
             [else (error 'assoc->object "invalid key value")])))
       ($/:= ($ result key) (car (cdr p)))
       (loop (cdr pairs))])))

(define-sequence-syntax *in-js-array
  (lambda () #'in-js-array)
  (lambda (stx)
    (syntax-case stx ()
      [[(id) (_ arr-expr)]
       #'[(id)
          (:do-in
           ;; outer bindings
           ([(arr) arr-expr])

           ;; outer check
           (unless (js-array? arr) (in-js-array arr))

           ;; loop bindings
           ([i 0])

           ;; position check
           (< i ($ arr 'length))

           ;; inner bindings
           ([(id) ($ arr i)])

           ;; pre guard
           #true

           ;; post guard
           #true

           ;; loop args
           [($/binop + i 1)])]])))

#;(define (unsafe-js-array-length arr)
  ($ arr 'length))

#;(define (unsafe-js-array-ref arr i)
  ($ arr 'length i))

(define (js-array? v)
  (($ ($ 'Array) 'isArray) v))

(define (in-js-array arr)
  (check-array arr)
  (for/list ([v (*in-js-array arr)]) v))

(define (check-array v)
  (unless (js-array? v)
    (raise-argument-error 'in-js-array "js-array?" v)))

(define-sequence-syntax *in-js-object
  (lambda () #'in-js-array)
  (lambda (stx)
    (syntax-case stx ()
      [[(key val) (_ obj-expr)]
       #'[(key val)
          (:do-in
           ;; outer bindings
           ([(obj) obj-expr]
            [(keys) (($ ($ 'Object) 'keys) obj-expr)])

           ;; outer check
           (unless (js-object? obj) (*in-js-object obj))

           ;; loop bindings
           ([i 0])

           ;; position check
           (< i ($ keys 'length))

           ;; inner bindings
           ([(key) ($ keys i)]
            [(val) ($ obj ($ keys i))])

           ;; pre guard
           #true

           ;; post guard
           #true

           ;; loop args
           [($/binop + i 1)])]])))

(define (in-js-obect obj)
  (check-object obj)
  (for/list ([(k v) (*in-js-object obj)]) (values k v)))

(define (js-object? v)
  ($/binop &&
    ($/binop &&
      ($/typeof v "object")
      ($/binop !== v $/null))
    (not (($ ($ '$rjs_core) 'Primitive 'check) v))))

(define (check-object v)
  (unless (js-object? v)
    (raise-argument-error 'in-js-object "js-object?" v)))

(define-syntax (for/js-array stx)
  (syntax-parse stx
    [(_ clauses body ... tail-expr)
     #:with original-stx stx
     #'(for/fold/derived original
                         ([result ($/array)])
                         clauses
                         body ...
                         (define iter-result tail-expr)
                         ($$ result.push iter-result)
                         result)]))

(define-syntax (for/js-object stx)
  (syntax-parse stx
    [(_ clauses body ... tail-expr)
     #:with original-stx stx
     #'(for/fold/derived original
                         ([result ($/obj)])
                         clauses
                         body ...
                         (let-values ([(k v) tail-expr])
                           ($/:= ($ result k) v))
                         result)]))

(module+ test
  (require rackunit)

  (define-simple-check (check-interop expr expected)
    (equal? (syntax->datum (expand expr))
            (syntax->datum (expand expected))))

  (define-simple-check (check-interop-exn expr)
    (with-handlers ([exn:fail? (λ (e) #t)])
      (and (expand expr) #f)))


  ;; Checks for `-$`
  (check-interop #'($ 'window) #'(#%js-ffi 'var 'window))
  (check-interop #'($ 'win91) #'(#%js-ffi 'var 'win91))
  (check-interop #'($ 'win91) #'(#%js-ffi 'var 'win91))
  (check-interop #'($ 'win$91) #'(#%js-ffi 'var 'win$91))
  (check-interop #'($ 'win_91) #'(#%js-ffi 'var 'win_91))

  (check-interop-exn #'(-$ 'window-manager) "invalid `-` character in between")
  (check-interop-exn #'(-$ 'window.manager) "invalid `.` character in between")

  (check-interop #'(-$ window 'document)
                 #'(#%js-ffi 'ref window 'document))
  (check-interop #'(-$ window 'document-wrong)
                 #'(#%js-ffi 'ref window 'document-wrong))
  (check-interop #'(-$ window "document-wrong")
                 #'(#%js-ffi 'index window "document-wrong"))

  ;; Check `$`
  (check-interop #'($ 'window) #'(#%js-ffi 'var 'window))
  (check-interop #'($ 'win91) #'(#%js-ffi 'var 'win91))
  (check-interop #'($ 'win91) #'(#%js-ffi 'var 'win91))
  (check-interop #'($ 'win$91) #'(#%js-ffi 'var 'win$91))
  (check-interop #'($ 'win_91) #'(#%js-ffi 'var 'win_91))

  (check-interop-exn #'($ 'window-manager) "invalid `-` character in between")
  (check-interop-exn #'($ 'window.manager) "invalid `.` character in between")

  (check-interop #'($ window 'document) #'(#%js-ffi 'ref window 'document))
  (check-interop #'($ window 'document 'write)
                 #'(#%js-ffi 'ref (#%js-ffi 'ref window 'document) 'write))
  (check-interop #'($ window "document")
                 #'(#%js-ffi 'index window "document"))
  (check-interop #'($ window 'window.manager)
                 #'(#%js-ffi 'ref window 'window.manager))

  (check-interop #'($ window 'document "write")
                 #'(#%js-ffi 'index (#%js-ffi 'ref window 'document) "write"))

  ;; Check `$$`
  (check-interop #'($$ 'window.document write)
                 #'((#%js-ffi 'ref 'window 'document) write))

  (check-interop #'($$ window.parent location)
                 #'((#%js-ffi 'ref window 'parent) location))

  (check-interop #'($$ 'window document write)
                 #'((#%js-ffi 'var 'window) document write))

  ;; Check $/new
  (check-interop #'($/new (Img "foo.jpeg")) #'(#%js-ffi 'new (Img "foo.jpeg")))

  ;; Check $/throw
  (check-interop #'($/throw '42) #'(#%js-ffi 'throw '42))

  ;; Check $/undefined
  (check-interop #'($/undefined) #'(#%js-ffi 'undefined))

  ;; Check $/null
  (check-interop #'($/null) #'(#%js-ffi 'null))

  ;; Check $/this
  (check-interop #'($/this) #'(#%js-ffi 'this))

  ;; Check $/arguments
  (check-interop #'($/arguments) #'(#%js-ffi 'arguments))

  ;; Check $/:=
  (check-interop #'($/:= ($ window 'document 'width) '42)
                 #'(#%js-ffi 'assign (#%js-ffi 'ref (#%js-ffi 'ref window 'document) 'width) '42))

  ;; Check $/array
  (check-interop #'($/array 42 'foobar ($/new ($/this)))
                 #'(#%js-ffi 'array 42 'foobar (#%js-ffi 'new (#%js-ffi 'this))))

  ;; Check $/require
  (check-interop #'($/require "foo.rkt") #'(#%js-ffi 'require "foo.rkt"))
  (check-interop #'($/require "bar.rkt" *) #'(#%js-ffi 'require '* "bar.rkt"))

  ;; Check $/require/*
  (check-interop #'($/require/* "foo.rkt") #'(#%js-ffi 'require '* "foo.rkt"))

  ;; Check $/typeof
  (check-interop #'($/typeof '42) #'(#%js-ffi 'typeof '42))
  (check-interop #'($/typeof '42 "function")
                 #'(#%js-ffi 'operator '=== (#%js-ffi 'typeof '42) (#%js-ffi 'string "function")))

  ;; Check $/instanceof
  (check-interop #'($/instanceof '42 'Object) #'(#%js-ffi 'instanceof '42 'Object))

  ;; Check $/binop
  (check-interop #'($/binop * '33 '67) #'(#%js-ffi 'operator '* '33 '67))

  ;; Check $/+
  (check-interop #'($/+ '5) #'5)
  (check-interop #'($/+ '5 '6 '7) #'(#%js-ffi 'operator '+ '5 (#%js-ffi 'operator '+ '6 '7)))

  ;; Check $/str
  (check-interop #'($/str "foobar") #'(#%js-ffi 'string "foobar"))
  (check-interop #'($/str (make-string)) #'(js-string (make-string)))

  ;; Check '$>'
  (check-interop #'($> foo bar (baz 'a 'b))
                 #'((#%js-ffi 'ref (#%js-ffi 'ref foo 'bar) 'baz) 'a 'b))

  ;; Object
  (check-interop #'($/obj
                    [x 12]
                    [y 13])
                 #'(#%js-ffi 'object 'x 'y 12 13))
  (check-interop #'($/obj
                    [x 12]
                    [y 13]
                    [active? #f]
                    ["feed?" #t])
                 #'(#%js-ffi 'object 'x 'y 'active? "feed?" 12 13 #f #t)))
