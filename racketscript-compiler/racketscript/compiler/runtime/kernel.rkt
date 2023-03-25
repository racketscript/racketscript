#lang racketscript/boot

(require (for-syntax syntax/parse)
         racketscript/interop
         racketscript/compiler/directive
         "lib.rkt")

;; ----------------------------------------------------------------------------
;; Equality

(define+provide equal? #js.Core.isEqual)
(define+provide eqv?   #js.Core.isEqv)
(define+provide eq?    #js.Core.isEq)

;; ----------------------------------------------------------------------------
;; Values

(define+provide values
  (#js.Core.attachProcedureName
   (#js.Core.attachProcedureArity
    (v-λ vals
         (if (binop === #js.vals.length 1)
             ($ vals 0)
             (#js.Values.make vals)))
    0)
   "values"))


(define+provide (call-with-values generator receiver)
  (let ([vals (generator)])
    (cond
      [(#js.Values.check vals)
       (#js.receiver.apply *this* (#js.vals.getAll))]
      [(not (or (eq? vals *undefined*) (eq? vals *null*)))
       (#js.receiver.apply *this* (array vals))])))

;; ----------------------------------------------------------------------------
;; Immutable

(define+provide (immutable? v)
  (#js.Kernel.isImmutable v))

;; ----------------------------------------------------------------------------
;; Void

(define+provide (void . _) *null*)

(define+provide (void? v)
  (or (binop === v *null*) (binop === v *undefined*)))

;; ----------------------------------------------------------------------------
;; Numbers

(define+provide number?   (if-scheme-numbers #js.Core.Number.Scheme.isSchemeNumber
                                             #js.Core.Number.JS.check))
(define+provide real?     (if-scheme-numbers #js.Core.Number.Scheme.isReal
                                             #js.Core.Number.JS.check))
(define+provide integer?  (if-scheme-numbers #js.Core.Number.Scheme.isInteger
                                             #js.Number.isInteger))

(define-syntax (define+provide/scheme-numbers stx)
  (syntax-parse stx
    [(_ identifier:id binding:expr)
     #`(define+provide identifier
         (if-scheme-numbers binding
                            (lambda _
                              (#js.Core.racketCoreError "Not supported with JS number semantics"))))]
    [(_ (identifier:id args:expr ...) body:expr ...)
     #'(define+provide/scheme-numbers identifier
         (λ (args ...) body ...))]))

(define-syntax (define-checked+provide/scheme-numbers stx)
  (syntax-parse stx
    [(_ (identifier:id [arg:expr pred:expr] ...) body:expr)
     #`(define-checked+provide (identifier [arg pred] ...)
         (if-scheme-numbers body
                            (lambda _
                              (#js.Core.racketCoreError "Not supported with JS number semantics"))))]))

(define+provide/scheme-numbers complex? number?)

(define-checked+provide (zero? [v number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.equals #js.Core.Number.Scheme.zero v)
                     (binop === v 0)))

(define-checked+provide (positive? [v real?])
  (if-scheme-numbers (#js.Core.Number.Scheme.greaterThan v #js.Core.Number.Scheme.zero)
                     (binop > v 0)))

(define-checked+provide (negative? [v real?])
  (if-scheme-numbers (#js.Core.Number.Scheme.lessThan v #js.Core.Number.Scheme.zero)
                     (binop < v 0)))

(define-checked+provide (add1 [v number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.add v #js.Core.Number.Scheme.one)
                     (binop + v 1)))

(define-checked+provide (sub1 [v number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.subtract v #js.Core.Number.Scheme.one)
                     (binop - v 1)))

(define-checked+provide (quotient [dividend integer?] [divisor integer?])
  (if-scheme-numbers (#js.Core.Number.Scheme.quotient dividend divisor)
                     (binop \| (binop / dividend divisor) 0)))

(define-checked+provide (even? [v integer?])
  (if-scheme-numbers (#js.Core.Number.Scheme.equals (#js.Core.Number.Scheme.modulo v 2) 0)
                     (binop === (binop % v 2) 0)))

(define-checked+provide (odd? [v integer?])
  (not (even? v)))

(define+provide (exact-nonnegative-integer? v)
  (if-scheme-numbers (and (integer? v)
                          (#js.Core.Number.Scheme.greaterThanOrEqual v 0)
                          (exact? v))
                     (and (#js.Number.isInteger v) (binop >= v 0))))

(define+provide (exact-integer? v)
  (if-scheme-numbers (and (integer? v)
                          (exact? v))
                     (#js.Number.isInteger v)))

(define+provide (exact? v)
  (if-scheme-numbers (#js.Core.Number.Scheme.isExact v)
                     (#js.Number.isInteger v)))

(define+provide (inexact? v) (not (exact? v)))

;; single-flonum not implemented
(define+provide (single-flonum-available?) (if-scheme-numbers #f
                                                              #f))
(define+provide (single-flonum?) (if-scheme-numbers #f
                                                    #f))
(define+provide (real->single-flonum v) (if-scheme-numbers v
                                                           v))

(define+provide *
  (if-scheme-numbers (#js.Core.attachProcedureArity (λ nums
                                                      (cond
                                                        [(null? nums)
                                                         1]
                                                        [(null? (cdr nums))
                                                         (car nums)]
                                                        [else
                                                         (#js.Core.Number.Scheme.multiply (car nums) (apply * (cdr nums)))]))
                      0)
                     (#js.Core.attachProcedureArity #js.Core.Number.JS.mul 0)))
(define+provide /
  (if-scheme-numbers (#js.Core.attachProcedureArity (λ nums
                                                      (cond
                                                        [(null? (cdr nums))
                                                         (#js.Core.Number.Scheme.divide 1 (car nums))]
                                                        [else
                                                         (foldl (λ (x y)
                                                                  (#js.Core.Number.Scheme.divide y x))
                                                                (car nums)
                                                                (cdr nums))]))
                      1)
                     (#js.Core.attachProcedureArity #js.Core.Number.JS.div 1)))
(define+provide +
  (if-scheme-numbers (#js.Core.attachProcedureArity (λ nums
                                                      (cond
                                                        [(null? nums)
                                                         0]
                                                        [(null? (cdr nums))
                                                         (car nums)]
                                                        [else
                                                         (#js.Core.Number.Scheme.add (car nums) (apply + (cdr nums)))]))
                      0)
                     (#js.Core.attachProcedureArity #js.Core.Number.JS.add 0)))
(define+provide -
  (if-scheme-numbers (#js.Core.attachProcedureArity (λ nums
                                                      (cond
                                                        [(null? (cdr nums))
                                                         (#js.Core.Number.Scheme.subtract 0 (car nums))]
                                                        [else
                                                         (foldl (λ (n acc)
                                                                  (#js.Core.Number.Scheme.subtract acc n))
                                                                (car nums)
                                                                (cdr nums))]))
                      1)
                     (#js.Core.attachProcedureArity #js.Core.Number.JS.sub 1)))

(define-syntax-rule (make-js-comparison op jsop)
  (λ nums
    (cond
      [(null? nums)
       #t]
      [(null? (cdr nums))
       #t]
      [else
       (and (jsop (car nums) (car (cdr nums)))
            (apply op (cdr nums)))])))

(define+provide <
  (if-scheme-numbers (#js.Core.attachProcedureArity (make-js-comparison < #js.Core.Number.Scheme.lessThan) 1)
                     (#js.Core.attachProcedureArity #js.Core.Number.JS.lt 1)))
(define+provide >
  (if-scheme-numbers (#js.Core.attachProcedureArity (make-js-comparison > #js.Core.Number.Scheme.greaterThan) 1)
                     (#js.Core.attachProcedureArity #js.Core.Number.JS.gt 1)))
(define+provide <=
  (if-scheme-numbers (#js.Core.attachProcedureArity (make-js-comparison <= #js.Core.Number.Scheme.lessThanOrEqual) 1)
                     (#js.Core.attachProcedureArity #js.Core.Number.JS.lte 1)))
(define+provide >=
  (if-scheme-numbers (#js.Core.attachProcedureArity (make-js-comparison >= #js.Core.Number.Scheme.greaterThanOrEqual) 1)
                     (#js.Core.attachProcedureArity #js.Core.Number.JS.gte 1)))
(define+provide =
  (if-scheme-numbers (#js.Core.attachProcedureArity (make-js-comparison = #js.Core.Number.Scheme.equals) 1)
                     (#js.Core.attachProcedureArity #js.Core.Number.JS.equals 1)))

(define-checked+provide (floor [v real?])
  (if-scheme-numbers (#js.Core.Number.Scheme.floor v)
                     (#js.Math.floor v)))
(define-checked+provide (abs [v real?])
  (if-scheme-numbers (#js.Core.Number.Scheme.abs v)
                     (#js.Math.abs v)))
(define-checked+provide (sin [v number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.sin v)
                     (#js.Math.sin v)))
(define-checked+provide (cos [v number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.cos v)
                     (#js.Math.cos v)))
(define-checked+provide (tan [v number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.tan v)
                     (#js.Math.tan v)))
(define-checked+provide (asin [v number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.asin v)
                     (#js.Math.asin v)))
(define-checked+provide (acos [v number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.acos v)
                     (#js.Math.acos v)))
(define+provide atan
  (if-scheme-numbers (case-lambda
                       [(x) (#js.Core.Number.Scheme.atan x)]
                       [(x y) (#js.Core.Number.Scheme.atan2 x y)])
                     (case-lambda
                       [(v) (#js.Math.atan v)]
                       [(x y) (#js.Math.atan2 x y)])))

(define-checked+provide (ceiling [v real?])
  (if-scheme-numbers (#js.Core.Number.Scheme.ceiling v)
                     (#js.Math.ceil v)))
(define-checked+provide (round [v real?])
  (if-scheme-numbers (#js.Core.Number.Scheme.round v)
                     (#js.Math.round v)))

(define-checked+provide (min [a real?] [b real?])
  (if-scheme-numbers (cond
                       [(not (and (number? a)
                                  (number? b)))
                        #js.Core.Number.Scheme.nan]
                       [(#js.Core.Number.Scheme.lessThan a b) a]
                       [else b])
                     (#js.Math.min a b)))
(define-checked+provide (max [a real?] [b real?])
  (if-scheme-numbers (cond
                       [(not (and (number? a)
                                  (number? b)))
                        #js.Core.Number.Scheme.nan]
                       [(#js.Core.Number.Scheme.greaterThan a b) a]
                       [else b])
                     (#js.Math.max a b)))

(define-checked+provide (log [v number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.log v)
                     (#js.Math.log v)))

(define-checked+provide (exp [w number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.exp w)
                     (#js.Math.exp w)))

(define-checked+provide (expt [w number?] [z number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.expt w z)
                     (#js.Math.pow w z)))

(define-checked+provide (sqrt [v number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.sqrt v)
                     (#js.Math.sqrt v)))

(define-checked+provide (sqr [v number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.sqr v)
                     (* v v)))

(define-checked+provide (truncate [v number?])
  (if-scheme-numbers (cond
                       [(negative? v)
                        (- (floor v) 1)]
                       [else (floor v)])
                     (#js.Math.trunc v)))

(define-checked+provide (remainder [a integer?] [b integer?])
  (if-scheme-numbers (#js.Core.Number.Scheme.remainder a b)
                     (binop % a b)))

(define-checked+provide (number->string [n number?])
  (#js.Core.UString.makeMutable (#js.n.toString)))

;; TODO: only works for numbers < 32 bits
(define-checked+provide (arithmetic-shift [n integer?] [m integer?])
  (if-scheme-numbers (#js.Core.Number.Scheme.arithmeticShift n m)
                     (if (negative? m)
                         (binop >> n (- m))
                         (binop << n m))))

;;TODO: Support bignums
(define+provide (inexact->exact v) (if-scheme-numbers (#js.Core.Number.Scheme.toExact v)
                                                      v))
(define+provide (exact->inexact v) (if-scheme-numbers (#js.Core.Number.Scheme.toInexact v)
                                                      v))

;; complex Numbers
(define-checked+provide (make-rectangular [x real?] [y real?])
  (if-scheme-numbers (#js.Core.Number.Scheme.makeComplex x y)
                     (#js.Core.Pair.make x y)))


(define-checked+provide (make-polar [m real?] [a real?])
  (if-scheme-numbers (#js.Core.Number.Scheme.makeComplexPolar m a)
                     (error "Complex numbers not supported with JS numerber semantics")))

(define-checked+provide (real-part [z (if-scheme-numbers number? pair?)])
                    (if-scheme-numbers (#js.Core.Number.Scheme.realPart z)
                                       (#js.z.hd z)))
(define-checked+provide (imag-part [z (if-scheme-numbers number? pair?)])
                     (if-scheme-numbers (#js.Core.Number.Scheme.imaginaryPart z)
                                        (#js.z.tl z)))
(define-checked+provide/scheme-numbers (magnitude [x number?])
  (#js.Core.Number.Scheme.magnitude x))

(define-checked+provide/scheme-numbers (conjugate [x number?])
  (#js.Core.Number.Scheme.conjugate x))

(define-checked+provide/scheme-numbers (angle [x number?])
  (#js.Core.Number.Scheme.angle x))

(define+provide rational? (if-scheme-numbers #js.Core.Number.Scheme.isRational
                                             #js.Number.isInteger))
(define-checked+provide (numerator [x number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.numerator x)
                     x))
(define-checked+provide (denominator [x number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.denominator x)
                     1))

;; bitwise operators

(define+provide bitwise-and
  (if-scheme-numbers (#js.Core.attachProcedureName
                      (#js.Core.attachProcedureArity #js.Core.Number.Scheme.bitwiseAnd 1)
                      "bitwise-and")
                     (#js.Core.attachProcedureName
                      (#js.Core.attachProcedureArity #js.Core.Number.JS.bitwiseAnd 1)
                      "bitwise-and")))

(define+provide bitwise-ior
  (if-scheme-numbers (#js.Core.attachProcedureName
                      (#js.Core.attachProcedureArity #js.Core.Number.Scheme.bitwiseOr 1)
                      "bitwise-ior")
                     (#js.Core.attachProcedureName
                      (#js.Core.attachProcedureArity #js.Core.Number.JS.bitwiseOr 1)
                      "bitwise-ior")))

(define+provide bitwise-xor
  (if-scheme-numbers (#js.Core.attachProcedureName
                      (#js.Core.attachProcedureArity #js.Core.Number.Scheme.bitwiseXor 1)
                      "bitwise-xor")
                     (#js.Core.attachProcedureName
                      (#js.Core.attachProcedureArity #js.Core.Number.JS.bitwiseXor 1)
                      "bitwise-xor")))

(define-checked+provide (bitwise-not [v number?])
  (if-scheme-numbers (#js.Core.Number.Scheme.bitwiseNot v)
                     (#js.Core.Number.JS.bitwiseNot v)))

(define+provide (bitwise-bit-set? n m)
  (not (zero? (bitwise-and n (arithmetic-shift 1 m)))))
;; ----------------------------------------------------------------------------
;; Booleans

(define+provide (not v)
  (or (equal? v #f) #f))

(define+provide false #f)
(define+provide true #t)

(define+provide (false? v) (binop === v #f))

(define+provide (boolean? v)
  (or (binop === v #t) (binop === v #f)))

;; ----------------------------------------------------------------------------
;; Pairs

(define-checked+provide (car [pair pair?]) #js.pair.hd)
(define-checked+provide (cdr [pair pair?]) #js.pair.tl)
(define+provide cons
  (#js.Core.attachProcedureName #js.Core.Pair.make "cons"))
(define+provide pair?
  (#js.Core.attachProcedureName #js.Core.Pair.check "pair?"))

(define-checked+provide (caar [v (check/pair-of? pair? #t)])
  #js.v.hd.hd)
(define-checked+provide (cadr [v (check/pair-of? #t pair?)])
  #js.v.tl.hd)
(define-checked+provide (cdar [v (check/pair-of? pair? #t)])
  #js.v.hd.tl)
(define-checked+provide (cddr [v (check/pair-of? #t pair?)])
  #js.v.tl.tl)
(define-checked+provide (cdddr [v (check/pair-of? #t (check/pair-of? #t pair?))])
  #js.v.tl.tl.tl)
(define-checked+provide (caddr [v (check/pair-of? #t (check/pair-of? #t pair?))])
  #js.v.tl.tl.hd)
(define-checked+provide (cadddr [v (check/pair-of? #t (check/pair-of? #t (check/pair-of? #t pair?)))])
  #js.v.tl.tl.tl.hd)
(define-checked+provide (cddddr [v (check/pair-of? #t (check/pair-of? #t (check/pair-of? #t pair?)))])
  #js.v.tl.tl.tl.tl)

(define+provide null #js.Core.Pair.EMPTY)
(define+provide list
  (#js.Core.attachProcedureName
   (#js.Core.attachProcedureArity #js.Core.Pair.makeList 0)
   "list"))

(define+provide null? #js.Core.Pair.isEmpty)
(define+provide list? #js.Core.Pair.isList)

(define-checked+provide (length [v list?]) #js.v.length)

(define-checked+provide (reverse [lst list?])
  (let loop ([lst lst]
             [result '()])
    (if (null? lst)
        result
        (loop #js.lst.tl (#js.Core.Pair.make #js.lst.hd result)))))

(define+provide list*
  (v-λ () #:unchecked
    ;; TODO: check at-least one argument given
    (define n-args #js.arguments.length)
    (define top-arguments arguments) ;;TODO: Make this explicit bound like `this`
    (let loop ([ii (binop - n-args 1)]
               [result ($ arguments (binop - n-args 1))])
      (cond
        [(binop === ii 0) result]
        [else
         (define next-ii (binop - ii 1))
         (loop next-ii
               (#js.Core.Pair.make ($ top-arguments next-ii) result))]))))

(define+provide append
  (v-λ () #:unchecked
    (define result '())
    (define lsts arguments)
    (for/array [lst lsts]
      (set! result (foldr #js.Core.Pair.make lst result)))
    result))

(define+provide for-each
  (v-λ (lam . lsts)
    (check/raise procedure? lam 0)
    (#js.map.apply *null* ($> (array lam) (concat lsts)))
    *null*))

;; ----------------------------------------------------------------------------
;; Mutable Pairs

(define+provide (mcons hd tl)
  (#js.Core.MPair.make hd tl))

(define+provide (mpair? v)
  (#js.Core.MPair.check v))

(define-checked+provide (mcar [p mpair?])
  (#js.p.car))

(define-checked+provide (mcdr [p mpair?])
  (#js.p.cdr))

(define+provide (set-mcar! p v)
  (check/raise mpair? p 0)
  (#js.p.setCar v))

(define+provide (set-mcdr! p v)
  (check/raise mpair? p 0)
  (#js.p.setCdr v))

;; --------------------------------------------------------------------------
;; Structs

(define+provide make-struct-type
  (v-λ (name
        super-type
        init-field-count
        auto-field-count
        auto-v
        props
        inspector
        proc-spec
        immutables
        guard
        constructor-name) #:unchecked
    ;;TODO: Add arity check
    (#js.Core.Struct.makeStructType
     {object [name (#js.name.toString)]
             [superType super-type]
             [initFieldCount init-field-count]
             [autoFieldCount auto-field-count]
             [autoV auto-v]
             [props props]
             [inspector inspector]
             [procSpec proc-spec]
             [immutables immutables]
             [guard guard]
             [constructorName constructor-name]})))

(define+provide make-struct-field-accessor
  (v-λ (ref index field-name) #:unchecked
    (v-λ (s)
      (ref s index))))

(define+provide make-struct-field-mutator
  (v-λ (set index fieldName) #:unchecked
    (v-λ (s v)
      (set s index v))))

(define+provide make-struct-type-property
  (v-λ (name guard supers can-impersonate?) #:unchecked
    (#js.Core.Struct.makeStructTypeProperty
     {object [name name]
             [guard guard]
             [supers supers]
             [canImpersonate can-impersonate?]})))

(define+provide (check-struct-type name what)
  (when what
    (unless (#js.Core.Struct.isStructType what)
      (throw (#js.Core.racketCoreError "not a struct type")))
    what))

(define+provide (struct-type? v)
  (#js.Core.Struct.isStructType v))

(define+provide (struct-type-info desc)
  (#js.Core.Values.make (#js.Core.Struct.structTypeInfo desc)))

;; --------------------------------------------------------------------------
;; Vectors

(define+provide vector
  (v-λ () #:unchecked
     (#js.Core.Vector.make (#js.Core.argumentsToArray arguments) #t)))

;; v is optional
(define-checked+provide (make-vector [size integer?] [v #t])
  (#js.Core.Vector.makeInit size
    (if (eq? v *undefined*)
      0
      v)))

(define+provide vector? #js.Core.Vector.check)

(define-checked+provide (vector-length [v vector?])
  (#js.v.length))

(define-checked+provide (vector-ref [vec vector?] [i integer?])
  (#js.vec.ref i))

(define-checked+provide (vector-set! [vec vector] [i integer?] [v #t])
  (#js.vec.set i v))

(define-checked+provide (vector->list [vec vector?])
  (#js.Core.Pair.listFromArray #js.vec.items))

(define-checked+provide (list->vector [lst list?])
  (#js.Core.Vector.make (#js.Core.Pair.listToArray lst) #t))

(define-checked+provide (vector->immutable-vector [vec vector?])
  (#js.Core.Vector.copy vec #f))

(define-checked+provide (vector-copy [vec vector?])
  (#js.Core.Vector.copy vec #t)) ; a vector copy is always mutable

(define-checked+provide (vector-copy! [dest vector?]
                                      [dest-start integer?]
                                      [src vector?]
                                      [src-start integer?]
                                      [src-end integer?])
  (#js.dest.copy dest-start src src-start src-end))

;; --------------------------------------------------------------------------
;; Hashes

(define-syntax-rule (make-hash-constructor make)
  (v-λ () #:unchecked
    (define kv* arguments)
    (when (binop !== (binop % #js.kv*.length 2) 0)
      (throw (#js.Core.racketContractError "invalid number of arguments")))
    (let ([items (array)])
      (loop+ [i 0 #js.kv*.length 2]
             (#js.items.push (array ($ kv* i) ($ kv* (+ i 1)))))
      (make items #f))))

(define+provide hash    (make-hash-constructor #js.Core.Hash.makeEqual))
(define+provide hasheqv (make-hash-constructor #js.Core.Hash.makeEqv))
(define+provide hasheq  (make-hash-constructor #js.Core.Hash.makeEq))

(define+provide make-hash
  (v-λ (assocs) #:unchecked
    (define assocs* (or assocs '()))
    (#js.Core.Hash.makeEqualFromAssocs assocs* #t)))
(define+provide make-hasheqv
  (v-λ (assocs) #:unchecked
    (define assocs* (or assocs '()))
    (#js.Core.Hash.makeEqvFromAssocs assocs* #t)))
(define+provide make-hasheq
  (v-λ (assocs) #:unchecked
    (define assocs* (or assocs '()))
    (#js.Core.Hash.makeEqFromAssocs assocs* #t)))

(define+provide make-immutable-hash
  (v-λ (assocs) #:unchecked
    (define assocs* (or assocs '()))
    (#js.Core.Hash.makeEqualFromAssocs assocs* #f)))
(define+provide make-immutable-hasheqv
  (v-λ (assocs) #:unchecked
    (define assocs* (or assocs '()))
    (#js.Core.Hash.makeEqvFromAssocs assocs* #f)))
(define+provide make-immutable-hasheq
  (v-λ (assocs) #:unchecked
    (define assocs* (or assocs '()))
    (#js.Core.Hash.makeEqFromAssocs assocs* #f)))

(define+provide hash? #js.Core.Hash.check)
(define+provide hash-equal? #js.Core.Hash.isEqualHash)
(define+provide hash-eqv? #js.Core.Hash.isEqvHash)
(define+provide hash-eq? #js.Core.Hash.isEqHash)
(define+provide hash-weak? #js.Core.Hash.isWeakHash) ;; TODO: implement weak hashes

(define+provide hash-ref
  (case-lambda
    [(h k)
     (if (#js.h.hasKey k)
         (#js.h._h.get k)
         (raise (#js.Core.makeArgumentsError
                 "hash-ref" "no value found for key" "key" k)))]
    [(h k fail) (#js.h.ref k fail)]))

(define+provide hash-has-key?
  (v-λ (h k) #:unchecked
    (#js.h.hasKey k)))

(define+provide hash-ref-key
  (case-lambda
    [(h k)
     (if (#js.h.hasKey k)
         (#js.h.refKey k)
         (raise (#js.Core.makeArgumentsError
                 "hash-ref-key" "hash does not contain key" "key" k)))]
    [(h k fail) (#js.h.refKey k fail)]))

(define+provide (hash-set h k v)
  (if (#js.h.isImmutable)
      (#js.h.set k v)
      (raise (#js.Core.makeArgumentError
              "hash-set" "(and/c hash? immutable?)" 0 h k v))))

(define+provide (hash-remove h k)
  (if (#js.h.isImmutable)
      (#js.h.remove k)
      (raise (#js.Core.makeArgumentError
              "hash-remove" "(and/c hash? immutable?)" 0 h k))))

(define+provide hash-map
  (case-lambda
    [(h proc) (#js.Core.Hash.map h proc)]
    [(h proc try-order?) (#js.Core.Hash.map h proc)]))

(define+provide (hash-count h)
  (#js.h.size))

;; mutating operations
(define+provide (hash-remove! h k)
  (if (#js.h.isImmutable h)
      (raise (#js.Core.makeArgumentError
              "hash-remove!" "(and/c hash? (not/c immutable?))" 0 h k))
      (#js.h.doremove k)))

(define+provide (hash-set! h k v)
  (if (#js.h.isImmutable h)
      (raise (#js.Core.makeArgumentError
              "hash-set!" "(and/c hash? (not/c immutable?))" 0 h k v))
      (#js.h.doset k v)))

;; iteration
(define+provide (hash-iterate-first h)
  (#js.h.iterateFirst))

(define+provide (hash-iterate-next h i)
  (#js.h.iterateNext i))

(define+provide (hash-iterate-key h i)
  (#js.h.iterateKey i))

(define+provide (hash-iterate-value h i)
  (#js.h.iterateValue i))

(define+provide (hash-iterate-key+value h i)
  (#js.h.iterateKeyValue i))

(define+provide (hash-iterate-pair h i)
  (#js.h.iteratePair i))

;; set operations for hash tables
(define+provide (hash-keys-subset? h1 h2)
  (if (and (#js.Core.Hash.check h1) (#js.Core.Hash.check h2))
      (if (#js.h1.isSameType h2)
          (#js.h1.isKeysSubset h2)
          (raise (#js.Core.makeArgumentsError
                  "hash-keys-subset?"
                  "given hash tables do not use the same key comparison"
                  "first table" h1
                  "second table" h2)))
      #f))

(define+provide (hash-union h1 h2)
  (#js.h1.union h2))

(define+provide (hash-strong? h) #t)

;; --------------------------------------------------------------------------
;; Higher Order Functions

(define+provide apply
  (v-λ (lam . args) #:unchecked
    (check/raise procedure? lam 0)
    (define final-args
      (cond
        [(zero? #js.args.length)
         (throw (#js.Core.racketContractError "arity mismatch"))]
        [(equal? #js.args.length 1)
         (unless (null? ($ args 0))
           (type-check/raise #js.Core.Pair ($ args 0)))
         (#js.Core.Pair.listToArray ($ args 0))]
        [else
         (#js.args.concat (#js.Core.Pair.listToArray (#js.args.pop)))]))
    (#js.lam.apply *null* final-args)))

(define+provide map
  (v-λ (fn . lists)
    (check/raise procedure? fn 0)
    (when (<= #js.lists.length 0)
      (error 'map "need at-least two arguments"))
    (define lst-len (length ($ lists 0)))
    (for/array [v lists 1]
      (unless (eq? (length v) lst-len)
        (error 'map "all input lists must have equal length")))

    (define result (Array lst-len))
    (define args (Array #js.lists.length))
    (loop+ [result-i lst-len]
      (for/array [(lst-j lst) lists]
        (:= ($ args lst-j) #js.lst.hd)
        (:= ($ lists lst-j) #js.lst.tl))
      (:= ($ result result-i) (#js.fn.apply *null* args)))

    (#js.Core.Pair.listFromArray result)))

(define+provide foldl
  (v-λ (fn init . lists)
    (check/raise procedure? fn 0)
    (when (<= #js.lists.length 0)
      (error 'foldl "need at-least two arguments"))
    (define lst-len (length ($ lists 0)))
    (for/array [v lists 1]
      (unless (eq? (length v) lst-len)
        (error 'foldl "all input lists must have equal length")))

    (define result init)
    (define args (Array (binop + #js.lists.length 1)))
    (loop+ [result-i lst-len]
      (for/array [(lst-j lst) lists]
        (:= ($ args lst-j) #js.lst.hd)
        (:= ($ lists lst-j) #js.lst.tl))
      (:= ($ args #js.lists.length) result)
      (set! result (#js.fn.apply *null* args)))

    result))

(define+provide (_foldr fn init lists)
  (cond
    [(null? ($ lists 0)) init]
    [else
     (define args (Array (add1 #js.lists.length)))
     (for/array [(ii lst) lists]
       (:= ($ args ii) #js.lst.hd)
       (:= ($ lists ii) #js.lst.tl))
     (:= ($ args #js.lists.length) (_foldr fn init lists))
     (#js.fn.apply *null* args)]))

(define+provide foldr
  (v-λ (fn init . lists)
    (check/raise procedure? fn 0)
    (when (<= #js.lists.length 0)
      (error 'foldr "need at-least two arguments"))
    (define lst-len (length ($ lists 0)))
    (for/array [v lists 1]
      (unless (eq? (length v) lst-len)
        (error 'foldr "all input lists must have equal length")))
    (_foldr fn init lists)))

(define+provide range
  (case-lambda
    [(end) (range 0 end 1)]
    [(start end) (range start end (if (< start end) 1 -1))]
    [(start end step)
     (define result (array))
     (cond
       [(and (>= step 0) (< step end))
        (loop+ [i start end step]
          (#js.result.push i))]
       [(and (<= step 0) (< end start))
        (loop+ [i (- start) (- end) (- step)]
          (#js.result.push (- i)))])
     (#js.Core.Pair.listFromArray result)]))

;; proc is optional
(define+provide (remove v lst proc)
  (when (eq? proc *undefined*)
    (set! proc #js.Core.isEqual))
  (let loop ([result '()]
             [lst lst])
    (cond
      [(null? lst) (reverse result)]
      [else
       (when (proc v (car lst))
         (append (reverse result) (cdr lst)))
       (loop (cons (car lst) result) (cdr lst))])))

(define+provide (filter fn lst)
  (let loop ([result '()]
             [lst lst])
    (cond
      [(null? lst) (reverse result)]
      [(fn #js.lst.hd) (loop (#js.Core.Pair.make #js.lst.hd result)
                             #js.lst.tl)]
      [else (loop result #js.lst.tl)])))

(define+provide ormap
  (v-λ (fn . lists)
    (#js.foldl.apply *this*
                     ($> (array (v-λ args
                                  (define final-arg (#js.args.pop))
                                  (and (or final-arg
                                           (#js.fn.apply *null* args))
                                       #t))
                                #f)
                         (concat lists)))))

(define+provide andmap
  (v-λ (fn . lists)
    (#js.foldl.apply *this*
                     ($> (array (v-λ args
                                  (define final-arg (#js.args.pop))
                                  (and final-arg
                                       (#js.fn.apply *null* args)
                                       #t))
                                #t)
                         (concat lists)))))

;; TODO: add optional equal? pred
(define+provide (member v lst)
  (let loop ([lst lst])
    (cond
      [(null? lst) #f]
      [(#js.Core.isEqual v #js.lst.hd) lst]
      [else (loop #js.lst.tl)])))

(define+provide compose
  (v-λ procs
    (v-λ () #:unchecked
      (define result (#js.Core.argumentsToArray arguments))
      (define procs* (#js.procs.reverse))
      (for/array [p procs*]
        (set! result (#js.p.apply *null* result))
        (if (#js.Core.Values.check result)
            (set! result (#js.result.getAll))
            (set! result (array result))))
      (if (binop === #js.result.length 1)
          ($ result 0)
          (#js.Core.Values.make result)))))

(define+provide compose1
  (v-λ procs
    (v-λ (v) #:unchecked
      (define result v)
      (define procs* (#js.procs.reverse))
      (for/array [p procs*]
        (set! result (p result)))
      result)))

;; Lists

(define+provide (list-ref lst pos)
  (let loop ([i 0]
             [lst lst])
    (cond
      [(null? lst) (error 'list-ref? "insufficient elements")]
      [(binop === i pos) #js.lst.hd]
      [else (loop (binop + i 1) #js.lst.tl)])))

(define+provide (build-list n proc)
  (define arr (Array n))
  (loop+ [i n]
    (:= ($ arr i) (proc i)))
  (#js.Core.Pair.listFromArray arr))

(define+provide (make-list n v)
  (let loop ([result '()]
             [i 0])
    (if (binop === i n)
        result
        (loop (#js.Core.Pair.make v result) (binop + i 1)))))

(define+provide (flatten lst)
  (cond
    [(null? lst) lst]
    [(pair? lst) (append (flatten #js.lst.hd) (flatten #js.lst.tl))]
    [else (list lst)]))

(define+provide (assoc k lst)
  (let loop ([lst lst])
    (cond
      [(null? lst) #f]
      [(#js.Core.isEqual k #js.lst.hd.hd) #js.lst.hd]
      [else (loop #js.lst.tl)])))

(define+provide memv #js.Kernel.memv)
(define+provide memq #js.Kernel.memq)
(define+provide memf #js.Kernel.memf)
(define+provide findf #js.Kernel.findf)
(define+provide assv #js.Kernel.assv)
(define+provide assq #js.Kernel.assq)
(define+provide assf #js.Kernel.assf)
(define+provide alt-reverse reverse)

;; --------------------------------------------------------------------------
;; Strings

(define+provide string
  (#js.Core.attachProcedureName
   #js.Core.UString.makeMutableFromCharsVarArgs
   "string"))

(define+provide string-append
  (#js.Core.attachProcedureName
   #js.Core.UString.stringAppend
   "string-append"))

(define-checked+provide (string-ref [s string?] [i exact-nonnegative-integer?])
  (if (#js.s.isValidIndex i)
      (#js.s.charAt i)
      (raise
       (#js.Core.makeOutOfRangeError "string-ref" "string" s #js.s.length i))))

(define-checked+provide (string=? [sa string?] [sb string?])
  (#js.Core.UString.eq sa sb))

(define-checked+provide (string<? [sa string?] [sb string?])
  (#js.Core.UString.lt sa sb))

(define-checked+provide (string<=? [sa string?] [sb string?])
  (#js.Core.UString.lte sa sb))

(define-checked+provide (string>? [sa string?] [sb string?])
  (#js.Core.UString.gt sa sb))

(define-checked+provide (string>=? [sa string?] [sb string?])
  (#js.Core.UString.gte sa sb))

(define+provide string?
  (#js.Core.attachProcedureName #js.Core.UString.check "string?"))

(define+provide (fprintf out form . args)
  (apply #js.Kernel.fprintf (print-as-expression) out form args))

(define+provide (eprintf form . args)
  (apply #js.Kernel.fprintf (print-as-expression) (current-error-port) form args))

(define+provide (printf form . args)
  (apply #js.Kernel.fprintf (print-as-expression) (current-output-port) form args))

(define+provide (format form . args)
  (let ([out (open-output-string)])
       (apply fprintf out form args)
       (get-output-string out)))

(define+provide symbol? #js.Core.PrimitiveSymbol.check)
(define+provide keyword? #js.Core.Keyword.check)

(define+provide (make-string k [c #\nul])
  (#js.Core.UString.repeatChar k c))

(define+provide (list->string lst)
  (#js.Kernel.listToString lst))
(define+provide (string->list [str string?])
  (#js.Core.Pair.listFromArray (#js.Core.UString.toArray str)))

(define+provide (string->immutable-string [s string?])
  (#js.Core.UString.stringToImmutableString s))

(define-checked+provide (symbol->string [v symbol?])
  (#js.Core.UString.makeMutable (#js.v.toString)))

(define-checked+provide (string->symbol [s string?])
  (#js.Core.PrimitiveSymbol.make s))

(define-checked+provide (string->uninterned-symbol [s string?])
  (#js.Core.PrimitiveSymbol.makeUninterned s))

;; TODO: implement unreadable symbols
(define-checked+provide (string->unreadable-symbol [s string?])
  (#js.Core.PrimitiveSymbol.make s))

; Does not support prefixed forms such as "#b101".
(define+provide (string->number s [radix 10])
  (define (integer-in lo hi)
    (v-λ (v) #:unchecked
         (and (exact-integer? v)
              (>= v 2)
              (<= v 16))))

  (check/raise string? s 0)
  (check/raise (integer-in 2 16) radix 1)

  (define (js-string->number)
    (let ([result (#js*.parseInt s radix)])
      (if (or (#js*.isNaN result)
              ; Work around parseInt permissiveness.
              (not (#js.s.isValidInteger radix)))
          #f
          result)))

  (if-scheme-numbers
   (let ([scheme-number (#js.Core.Number.Scheme.fromString s)])
     (if (and scheme-number
              (= radix 10))
         scheme-number
         (js-string->number)))
   (js-string->number)))

(define-checked+provide (symbol-interned? [sym symbol?])
  (#js.Core.PrimitiveSymbol.isInterned sym))

(define+provide (symbol=? s v)
  (#js.s.equals v))

(define+provide (symbol<? s v)
  (#js.s.lt v))

(define+provide (keyword<? s v)
  (#js.s.lt v))

(define-checked+provide (string-length [s string?])
  #js.s.length)

(define-checked+provide (string-downcase [s string?])
  (#js.s.toLowerCase))

(define-checked+provide (string-upcase [s string?])
  (#js.s.toUpperCase))

(define+provide (substring str start [end #f])
  (cond
    [(not (#js.Core.UString.check str))
     (throw (#js.Core.racketContractError "expected a string"))]
    [(binop < start 0)
     (throw (#js.Core.racketContractError "invalid start index"))]
    [(and (binop !== end #f)
               (or (binop < end 0)
                   (binop > end #js.str.length)
                   (binop < end start)))
     (throw (#js.Core.racketContractError "invalid end index"))]
    [(binop === end #f)
     (set! end #js.str.length)])
  (#js.str.substring start end))

;; TODO: Should this be in the Kernel? It's not in the Racket kernel.
(define-checked+provide (string-split [str string?] [sep (check/or string? regexp?)])
  (#js.Core.Pair.listFromArray (#js.str.split sep)))

;; Mutable string methods

(define-checked+provide (string-set!
                         [str (check/and string? (check/not immutable?))]
                         [k exact-nonnegative-integer?]
                         [char char?])
  (if (#js.str.isValidIndex k)
      (#js.str.setCharAt k char)
      (raise
       (#js.Core.makeOutOfRangeError "string-set!" "string" str #js.str.length k))))

;; --------------------------------------------------------------------------
;; Characters

(define+provide (char? c)
  (#js.Core.Char.check c))

(define-checked+provide (char->integer [c char?])
  (#js.Core.Char.charToInteger c))

(define-checked+provide (integer->char [k exact-nonnegative-integer?])
  (#js.Core.Char.integerToChar k))

(define-checked+provide (char-utf-8-length [c char?])
  (#js.Core.Char.charUtf8Length c))

(define-checked+provide (char-upcase [c char?])
  (#js.Core.Char.upcase c))

(define-checked+provide (char-downcase [c char?])
  (#js.Core.Char.downcase c))

(define-checked+provide (char-alphabetic? [c char?])
  (#js.Core.Char.isAlphabetic c))

(define-checked+provide (char-lower-case? [c char?])
  (#js.Core.Char.isLowerCase c))

(define-checked+provide (char-upper-case? [c char?])
  (#js.Core.Char.isUpperCase c))

(define-checked+provide (char-title-case? [c char?])
  (#js.Core.Char.isTitleCase c))

(define-checked+provide (char-numeric? [c char?])
  (#js.Core.Char.isNumeric c))

(define-checked+provide (char-symbolic? [c char?])
  (#js.Core.Char.isSymbolic c))

(define-checked+provide (char-punctuation? [c char?])
  (#js.Core.Char.isPunctuation c))

(define-checked+provide (char-graphic? [c char?])
  (#js.Core.Char.isGraphic c))

(define-checked+provide (char-whitespace? [c char?])
  (#js.Core.Char.isWhitespace c))

(define-checked+provide (char-blank? [c char?])
  (#js.Core.Char.isBlank c))

(define-checked+provide (char-iso-control? [c char?])
  (#js.Core.Char.isIsoControl c))

; TODO: Add varargs support for the comparison methods below.

(define-checked+provide (char<? [a char?] [b char?])
  (binop < a b))

(define-checked+provide (char<=? [a char?] [b char?])
  (binop <= a b))

(define-checked+provide (char>? [a char?] [b char?])
  (binop > a b))

(define-checked+provide (char>=? [a char?] [b char?])
  (binop >= a b))

(define-checked+provide (char=? [a char?] [b char?])
  (#js.Core.Char.eq a b))

;; --------------------------------------------------------------------------
;; Box

(define+provide box #js.Core.Box.make)

(define+provide (unbox v)
  (#js.v.get))

(define+provide (set-box! b v)
  (#js.b.set v))

(define+provide box? #js.Core.Box.check)

;; FIXME below here
(define+provide (box-cas! loc old new)
  ;; doesn't handle threads
  (and (eq? old (unbox loc)) (set-box! loc new) #t))

(define+provide box-immutable #js.Core.Box.make)

(define+provide make-weak-box #js.Core.Box.make)
(define+provide (weak-box-value v) (#js.v.get))

(define+provide (set-box*! b v) (#js.b.set v))
(define+provide (unbox* v) (#js.v.get))

;; --------------------------------------------------------------------------
;; Properties

(define-syntax (define-property+provide stx)
  (syntax-parse stx
    [(_ name:id)
     #`(begin
         (provide name)
         (define+provide name
           (($ (make-struct-type-property
                #,(symbol->string (syntax-e #'name))) 'getAt)
            0)))]))

(provide prop:evt evt?)
(define-values (prop:evt evt?) (#js.Core.Struct.makeStructTypeProperty
                                {object [name "prop:evt"]}))

(define-property+provide prop:checked-procedure)
(define-property+provide prop:impersonator-of)
(define-property+provide prop:arity-string)
(define-property+provide prop:incomplete-arity)
(define-property+provide prop:method-arity-error)
(define-property+provide prop:exn:srclocs)
(define-property+provide prop:authentic)
(define-property+provide prop:serialize)
(define-property+provide prop:custom-write)
(define-property+provide prop:sealed)
(define-property+provide prop:object-name)

(define+provide prop:procedure #js.Core.Struct.propProcedure)
(define+provide prop:equal+hash #js.Core.Struct.propEqualHash)

(define+provide (equal-hash-code v) 0)
(define+provide (equal-secondary-hash-code v) 1)

;; --------------------------------------------------------------------------
;; Errors

(define+provide error #js.Kernel.error)
(define+provide raise-argument-error #js.Kernel.argerror)
(define+provide raise-arguments-error #js.Kernel.argserror)
(define+provide raise-result-error #js.Kernel.resulterror)
(define+provide raise-mismatch-error #js.Kernel.mismatcherror)

;; --------------------------------------------------------------------------
;; Bytes

(define+provide (bytes? bs)
  (#js.Core.Bytes.check bs))

;; init val `b` is optional
(define+provide (make-bytes len [b 0])
  (#js.Core.Bytes.make len b))

(define-checked+provide (bytes-ref [bs bytes?] [i integer?])
  (if (or (< i 0) (> i #js.bs.length))
      (raise
       (#js.Core.makeOutOfRangeError "bytes-ref" "byte string" bs #js.bs.length i))
      (#js.Core.Bytes.ref bs i)))

(define-checked+provide (bytes-set! [bs bytes?] [i integer?] [b integer?])
  (if (or (< i 0) (> i #js.bs.length))
      (raise
       (#js.Core.makeOutOfRangeError "bytes-set!" "byte string" bs #js.bs.length i))
      (#js.Core.Bytes.set bs i b)))

(define+provide bytes-append
  (v-λ bss #:unchecked (#js.Core.Bytes.append bss)))

(define-checked+provide (bytes->string/utf-8 [bs bytes?])
  (#js.Core.UString.fromBytesUtf8 bs))

(define-checked+provide (bytes->string/latin-1 [bs bytes?])
  (#js.Core.UString.fromBytesLatin1 bs))

(define-checked+provide (string->bytes/utf-8 [str string?])
  (#js.Core.UString.toBytesUtf8 str))

(define+provide (string->bytes/locale str [err-byte #t] [start 0] [end 0])
  (#js.Core.UString.toBytesUtf8 str))

(define-checked+provide (bytes=? [bstr1 bytes?] [bstr2 bytes?])
  (#js.Core.Bytes.eq bstr1 bstr2))

(define-checked+provide (bytes<? [bstr1 bytes?] [bstr2 bytes?])
  (#js.Core.Bytes.lt bstr1 bstr2))

(define-checked+provide (bytes>? [bstr1 bytes?] [bstr2 bytes?])
  (#js.Core.Bytes.gt bstr1 bstr2))

(define-checked+provide (bytes-length [bs bytes?])
  #js.bs.length)

;; --------------------------------------------------------------------------
;; Continuation Marks

(define+provide current-continuation-marks   #js.Core.Marks.getContinuationMarks)
(define+provide continuation-mark-set->list  #js.Core.Marks.getMarks)

(define+provide continuation-mark-set-first
  (v-λ (mark-set key-v none-v prompt-tag) #:unchecked
    ;; TODO: implement prompt tag
    (define mark-set (or mark-set (#js.Core.Marks.getContinuationMarks prompt-tag)))
    (define marks (#js.Core.Marks.getMarks mark-set key-v prompt-tag))
    (if (null? marks)
        none-v
        #js.marks.hd)))

(define+provide make-parameter #js.Paramz.makeParameter)

(define+provide call-with-continuation-prompt
  #js.Core.Marks.callWithContinuationPrompt)

(define+provide abort-current-continuation
  (v-λ (prompt-tag . args)
       (throw (new (#js.Core.Marks.AbortCurrentContinuation prompt-tag args)))))

(define+provide make-continuation-prompt-tag
  #js.Core.Marks.makeContinuationPromptTag)
(define+provide default-continuation-prompt-tag
  #js.Core.Marks.defaultContinuationPromptTag)

(define+provide raise #js.Kernel.doraise)

(define+provide exn:fail? #js.Core.isErr)
(define+provide exn:fail:contract? #js.Core.isContractErr)
(define+provide exn:fail:contract:arity? #js.Core.isContractErr)
(define+provide (exn-message e)
  (#js.Core.UString.makeMutable (#js.Core.errMsg e)))

;; --------------------------------------------------------------------------
;; Ports + Writers

(define+provide current-output-port
  (make-parameter #js.Core.Ports.standardOutputPort))

(define+provide current-input-port
  (make-parameter #js.Core.Ports.standardInputPort))

(define+provide current-error-port
  (make-parameter #js.Core.Ports.standardErrorPort))

(define+provide current-print
  (make-parameter
    (v-λ (p) #:unchecked
      (unless (void? p)
        (print p)  ; can't use println here yet (it's defined by private/misc.rkt)
        (newline)))))

(define+provide (port? p)
  (#js.Core.Ports.check p))

(define+provide (input-port? p)
  (#js.Core.Ports.isInputPort p))

(define+provide (output-port? p)
  (#js.Core.Ports.isOutputPort p))

(define+provide (string-port? p)
  (#js.Core.Ports.isStringPort p))

(define+provide (open-output-string)
  (#js.Core.Ports.openOutputString))

(define+provide (get-output-string p)
  (#js.Core.Ports.getOutputString p))

;; --------------------------------------------------------------------------
;; Printing

(define+provide print-as-expression (make-parameter #t))

;;TODO: These compile to case-lambda's. Check performance and use unchecked
;;      lambdas.
(define+provide (display datum [out (current-output-port)])
  (#js.Core.display out datum))
(define+provide (displayln datum [out (current-output-port)])
  (display datum out)
  (newline out))
(define+provide (write datum [out (current-output-port)])
  (#js.Core.write out datum))
(define+provide (writeln datum [out (current-output-port)])
  (write datum out)
  (newline out))
(define+provide (print datum [out (current-output-port)] [quote-depth 0])
  (#js.Core.print out datum (print-as-expression) quote-depth))
(define+provide (println datum [out (current-output-port)])
  (print datum out)
  (newline out))

(define+provide (newline [out (current-output-port)])
  (display "\n" out)) ; TODO: should be write-char, but write doesnt work either

;; --------------------------------------------------------------------------
;; Not implemented/Unorganized/Dummies

(define+provide current-inspector (v-λ () #:unchecked #t))
(define+provide current-code-inspector (v-λ () #:unchecked #t))
(define+provide (make-inspector . _) #f)
(define+provide (check-method) #f)

(define+provide random #js.Kernel.random)

(define+provide (current-seconds)
  (#js.Math.floor (binop / (#js.Date.now) 1000)))

(define+provide (object-name fn) ;; TODO: what if not fn?
  #js.fn.name)
(define+provide (unquoted-printing-string s) s) ;; TODO
(define+provide (error-print-width) 42)
(define+provide (error-value->string-handler)
  (v-λ (x n)
       "str" #;(#js.x.toString)))

(define+provide (procedure-extract-target f) #f)

;; --------------------------------------------------------------------------
;; Regexp

;; TODO: both regexps and pregexps currently compile to js regexps,
;;       but js doesnt support posix patterns

(define+provide (regexp? v)
  (#js.Core.Regexp.check v))

(define+provide pregexp? regexp?)
(define+provide byte-regexp? regexp?)
(define+provide byte-pregexp? regexp?)

(define-checked+provide (regexp [str string?])
  (#js.Core.Regexp.fromString str))

(define+provide pregexp regexp)

(define-checked+provide (byte-regexp [bs bytes?])
  (#js.Core.Regexp.fromString (bytes->string/utf-8 bs)))

(define+provide byte-pregexp byte-regexp)

(define+provide (regexp-match pattern input [start-pos 0] [end-pos #f])
  (#js.Core.Regexp.match pattern input start-pos end-pos))

(define+provide (regexp-match? pattern input [start-pos 0] [end-pos #f])
  (if (#js.Core.Regexp.match pattern input start-pos end-pos)
      #t
      #f))

;; --------------------------------------------------------------------------
;; Procedures

;; This is prefixed with `kernel` because otherwise
;; it would clash with Racket's built-in.
(provide (struct-out kernel:arity-at-least))
(struct kernel:arity-at-least (value)
  #:extra-constructor-name make-arity-at-least
  #:transparent)

(define+provide (procedure? f)
  (typeof f "function"))

(define+provide arity-at-least make-arity-at-least)

(define+provide (arity-at-least? p)
  (kernel:arity-at-least? p))

(define+provide (arity-at-least-value p)
  (kernel:arity-at-least-value p))

(define+provide procedure-arity-includes?
  (v-λ (fn n) #:unchecked
       (let ([ar (procedure-arity fn)])
         (cond
           [(kernel:arity-at-least? ar) (<= (kernel:arity-at-least-value ar) n)]
           [(list? ar) (member n ar)]
           [else (binop === n ar)]))))

(define+provide (procedure-arity fn)
  (if (#js.Array.isArray #js.fn.__rjs_arityValue)
      (if (binop === #js.fn.__rjs_arityValue.length 1)
          ($ #js.fn.__rjs_arityValue 0)
          (#js.Core.Pair.listFromArray #js.fn.__rjs_arityValue))
      (if (binop === #js.fn.__rjs_arityValue *undefined*)
          #js.fn.length
          (kernel:arity-at-least (or #js.fn.__rjs_arityValue #js.fn.length)))))

(define+provide (procedure-arity? v)
  (or (exact-nonnegative-integer? v)
      (kernel:arity-at-least? v)
      (ormap (λ (v)
               (or (exact-nonnegative-integer? v)
                   (kernel:arity-at-least? v)))
             v)))

(define+provide (procedure-arity-mask fn)
  (let ([ar (procedure-arity fn)])
    (cond
      [(integer? ar)
       (arithmetic-shift 1 ar)]
      [(kernel:arity-at-least? ar)
       (arithmetic-shift -1 (kernel:arity-at-least-value ar))])))

(define+provide (checked-procedure-check-and-extract type v proc v1 v2)
  (cond
    [(and (#js.Core.Struct.check v type)
          (#js.type._findProperty prop:checked-procedure))
     (let* ([fn (#js.v.getField 0)]
            [r1 (fn v1 v2)])
       (if r1
           (#js.v.getField 1)
           (proc v v1 v2)))]
    [else (proc v v1 v2)]))
;; --------------------------------------------------------------------------
;;

(define+provide gensym
  (v-λ (sym) #:unchecked
    (let ([s (or (and sym #js.sym.v) "")])
      (set! __count (binop + __count 1))
      (#js.Core.PrimitiveSymbol.makeUninterned (binop + s __count)))))

(define+provide (eval-jit-enabled) #f)

(define+provide (variable-reference-constant? x) #f)
(define+provide (variable-reference-from-unsafe? x) #f)
(define+provide (variable-reference->module-source x) #f)
(define+provide (variable-reference->resolved-module-path x) #f)
(define+provide (module-name-fixup x) #f)

(define+provide (inspector? p)
  #t)

(define+provide (make-thread-cell p) p)

(define __count 1000)

(define+provide (system-type [mode 'os])
  (case mode
    [(os) 'unix]
    [(vm) 'javascript]
    [(gc) 'javascript]
    [(fs-change) (#js.Core.Vector.make (array #false #false #false #false) #false)]
    [else #false])
  )

;; path stubs
(define+provide (find-system-path kind)  "")
(define+provide build-path ; multi-arity
  (v-λ (base) #:unchecked base))

;; TODO: manually implement weak references? or ES6 WeakMap? see pr#106
(define+provide make-weak-hash make-hash)
(define+provide make-weak-hasheqv make-hasheqv)
(define+provide make-weak-hasheq make-hasheq)


(define+provide (current-environment-variables) null)
(define+provide (environment-variables-ref e n) #f)
(define+provide (environment-variables-set! e n v [fail #f]) (void))

(define+provide (prefab-struct-key v) #f)

(define+provide path? #js.Core.Path.check)

(define+provide (version) "99.0") ;; fake

(define+provide string->path #js.Core.Path.fromString)

;; --------------------------------------------------------------------------

(define+provide (dynamic-wind f g h)
  (f) (g) (h))

(define+provide (datum-intern-literal v) v)

;; semaphore stubs
(define+provide (make-semaphore x) x)
(define+provide (semaphore-peek-evt x) x)
(define+provide call-with-semaphore
  (v-λ (s f) #:unchecked #f))

;; ----------------------------------------------------------------------------
;; Syntax

;; TODO: implement these stubs

(define+provide syntax-source #js.Core.Correlated.syntaxSource)
(define+provide syntax-line #js.Core.Correlated.syntaxLine)
(define+provide syntax-column #js.Core.Correlated.syntaxColumn)
(define+provide syntax-position #js.Core.Correlated.syntaxPosition)
(define+provide syntax-span #js.Core.Correlated.syntaxSpan)
