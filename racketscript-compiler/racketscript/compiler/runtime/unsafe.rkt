#lang racketscript/boot

(require (for-syntax syntax/parse)
         racketscript/interop
         racketscript/compiler/directive
         "lib.rkt")

(define Core   ($/require/* "./core.js"))

;;-----------------------------------------------------------------------------
;; Unsafe Numeric Operations

(define-binop bitwise-or \|)

(define-syntax (define-unsafe-fx-binop+provide stx)
  (syntax-parse stx
    [(_ opname:id op:id)
     #'(begin
         (provide opname)
         (define+provide (opname a b)
           (bitwise-or (binop op a b) 0)))]))

(define-unsafe-fx-binop+provide unsafe-fx+         +)
(define-unsafe-fx-binop+provide unsafe-fx-         -)
(define-unsafe-fx-binop+provide unsafe-fx*         *)
(define-unsafe-fx-binop+provide unsafe-fxquotient  /)
(define-unsafe-fx-binop+provide unsafe-fxremainder %)

(define+provide (unsafe-fxmodulo a b)
  (define remainder (binop % a b))
  (#js.Math.floor (if (binop >= remainder 0)
                      remainder
                      (binop + remainder b))))

(define+provide (unsafe-fxabs a)
  (#js.Math.abs a))


(define+provide (unsafe-fx= a b)
  (binop === a b))
(define+provide (unsafe-fx< a b)
  (binop < a b))
(define+provide (unsafe-fx<= a b)
  (binop <= a b))
(define+provide (unsafe-fx> a b)
  (binop > a b))
(define+provide (unsafe-fx>= a b)
  (binop >= a b))
(define+provide (unsafe-fxmin a b)
  (if ($/binop < a b) a b))
(define+provide (unsafe-fxmax a b)
  (if ($/binop > a b) b a))

;; TODO: is this correct?
(define+provide (unsafe-fl= a b)
  (binop === a b))
(define+provide (unsafe-fl< a b)
  (binop < a b))
(define+provide (unsafe-fl<= a b)
  (binop <= a b))
(define+provide (unsafe-fl> a b)
  (binop > a b))
(define+provide (unsafe-fl>= a b)
  (binop >= a b))
(define+provide (unsafe-flmin a b)
  (if ($/binop < a b) a b))
(define+provide (unsafe-flmax a b)
  (if ($/binop > a b) b a))

(define-unsafe-fx-binop+provide unsafe-fxrshift  >>)
(define-unsafe-fx-binop+provide unsafe-fxlshift  <<)
(define-unsafe-fx-binop+provide unsafe-fxand     &&)
(define-unsafe-fx-binop+provide unsafe-fxior     \|\|)
(define-unsafe-fx-binop+provide unsafe-fxxor     ^)
(define+provide unsafe-fxnot                     #js.Core.bitwiseNot)

;;-----------------------------------------------------------------------------
;; UNSAFE DATA EXTRACTION
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Pairs

(define+provide (unsafe-car v) #js.v.hd)
(define+provide (unsafe-cdr v) #js.v.tl)
(define+provide (unsafe-mcar v) #js.v.hd)
(define+provide (unsafe-mcdr v) #js.v.tl)
(define+provide (unsafe-set-mcar! p v) (#js.p.setCar v))
(define+provide (unsafe-set-mcdr! p v) (#js.p.setCdr v))
(define+provide (unsafe-cons-list v rest)
  (#js.Core.Pair.make v rest))

;;-----------------------------------------------------------------------------
;; Structures

(define+provide (unsafe-struct-ref v k)
  ($ #js.v._fields k))

;;-----------------------------------------------------------------------------
;; Vector
(define+provide (unsafe-vector-ref v k)
  (#js.v.ref k))

(define+provide (unsafe-vector-set! v k val)
  (#js.v.set k val))

(define+provide (unsafe-vector-length v)
  (#js.v.length))

(define+provide (unsafe-vector*-ref v k)
  (#js.v.ref k))

(define+provide (unsafe-vector*-set! v k val)
  (#js.v.set k val))

(define+provide (unsafe-vector*-length v)
  (#js.v.length))

;;-----------------------------------------------------------------------------
;; Hash
(define+provide (unsafe-immutable-hash-iterate-first h)
  (#js.h.iterateFirst))

(define+provide (unsafe-immutable-hash-iterate-next h i)
  (#js.h.iterateNext i))

(define+provide (unsafe-immutable-hash-iterate-key h i)
  (#js.h.iterateKey i))

(define+provide (unsafe-immutable-hash-iterate-value h i)
  (#js.h.iterateValue i))

(define+provide (unsafe-immutable-hash-iterate-key+value h i)
  (#js.h.iterateKeyValue i))

(define+provide (unsafe-immutable-hash-iterate-pair h i)
  (#js.h.iteratePair i))

(define+provide (unsafe-mutable-hash-iterate-first h)
  (#js.h.iterateFirst))

(define+provide (unsafe-mutable-hash-iterate-next h i)
  (#js.h.iterateNext i))

(define+provide (unsafe-mutable-hash-iterate-key h i)
  (#js.h.iterateKey i))

(define+provide (unsafe-mutable-hash-iterate-value h i)
  (#js.h.iterateValue i))

(define+provide (unsafe-mutable-hash-iterate-key+value h i)
  (#js.h.iterateKeyValue i))

(define+provide (unsafe-mutable-hash-iterate-pair h i)
  (#js.h.iteratePair i))

(define+provide unsafe-undefined #js.Core.theUnsafeUndefined)

;; stubs
(define+provide unsafe-make-place-local #js.Core.Box.make)
(define+provide (unsafe-place-local-set! b v) (#js.b.set v))
(define+provide (unsafe-place-local-ref b) (#js.b.get))

(define+provide (variable-reference-from-unsafe? v) #f)

(define+provide (unsafe-root-continuation-prompt-tag)
  (#js.Core.Marks.defaultContinuationPromptTag))

;; strings
(define+provide (unsafe-string-length s)
  #js.s.length)

