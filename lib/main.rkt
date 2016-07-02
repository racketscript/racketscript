#lang racket/base

(provide $ #%js-ffi $/new)

(require (for-syntax syntax/parse
                     racket/string
                     racket/base
                     racket/sequence
                     syntax/stx
                     threading))

;; Invented out of thin air, #%js-ffi is treated specially by compiler
;; to do some JS things otherwise not possible
;;
;; #%js-ffi : Operation Any ... -> Any
;; WHERE:
;; - Operation is one of -
;;   + 'ref
;;   + 'index
;;   + 'app
;;   + 'var
;;   + 'assign
;;   + 'new
(define #%js-ffi
  (λ _
    (error 'rapture "can't make JS ffi calls in Racket")))

(begin-for-syntax
  (define (ids->sym stx)
    (stx-map (λ (c) #`'#,c) stx))

  (define-syntax-class symbol
    #:description "match with symbol datum"
    (pattern ((~literal quote) var:id))))

(define-syntax ($ stx)
  (syntax-parse stx
    [(_ v:id)
     #`(#%js-ffi 'var 'v)]
    ;; Symbols <: Expr so so just try to parse them first
    ;; Since symbols are static, we can use JS subscript syntax
    [(_ b:expr xs:symbol ...+ (~datum <$>) ys:expr ...)
     #`(#%js-ffi 'app (#%js-ffi 'ref b xs ...) ys ...)]
    [(_ b:expr xs:symbol ...+ (~datum <:=>) ys:expr)
     #`(#%js-ffi 'assign (#%js-ffi 'ref b xs ...) ys)]
    [(_ b:expr xs:symbol ...+)
     #`(#%js-ffi 'ref b xs ...)]
    ;; For everything else use JS indexing. They must
    ;; evaluate to String or something that JS can translate
    ;; to meaningful string
    [(_ b:expr xs:expr ...+ (~datum <:=>) ys:expr)
     #`(#%js-ffi 'assign (#%js-ffi 'index b xs ...) ys)]
    [(_ b:expr xs:expr ...+ (~datum <$>) ys:expr ...)
     #`(#%js-ffi 'app (#%js-ffi 'index b xs ...) ys ...)]
    [(_ b:expr xs:expr ...+)
     #`(#%js-ffi 'index b xs ...)]
    [_ (error '$ "no match")]))

(define-syntax ($/new stx)
  (syntax-parse stx
    [(_ v:expr)
     #`(#%js-ffi 'new v)]))
