#lang racket/base

(provide #%js-ffi
         $
         $/new
         $/obj
         $/array
         $/require
         $$
         $>
         $/:=
         assoc->object)

(require (for-syntax syntax/parse
                     racket/string
                     racket/base
                     racket/sequence
                     syntax/stx
                     threading
                     (for-template "private/interop.rkt")
                     "private/interop.rkt"))

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
     #`(#%js-ffi 'require mod)]))

(define-syntax ($> stx)
  (define-syntax-class chaincall
    (pattern [fieldname:id ρ:expr ...]))

  (syntax-parse stx
    [($> e:expr) #'e]
    [($> e:expr cc0:chaincall cc:chaincall ...)
     #'($> (($ e 'cc0.fieldname) cc0.ρ ...) cc ...)]))

(define (assoc->object pairs)
  (define result ($/obj))
  (let loop ([pairs pairs])
    (unless (null? pairs)
      (define p (car pairs))
      ($/:= ($ result (car p)) (car (cdr p)))
      (loop (cdr pairs)))))

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
