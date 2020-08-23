#lang racketscript/boot

(require racketscript/interop
         racket/stxparam
         (for-syntax syntax/parse)
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

(define+provide (void) *null*)

(define+provide (void? v)
  (or (binop === v *null*) (binop === v *undefined*)))

;; ----------------------------------------------------------------------------
;; Numbers

(define+provide number?   #js.Core.Number.check)
(define+provide real?     #js.Core.Number.check)
(define+provide integer?  #js.Number.isInteger)

(define-checked+provide (zero? [v number?])
  (binop === v 0))

(define-checked+provide (positive? [v real?])
  (binop > v 0))

(define-checked+provide (negative? [v real?])
  (binop < v 0))

(define-checked+provide (add1 [v number?])
  (binop + v 1))

(define-checked+provide (sub1 [v number?])
  (binop - v 1))

(define-checked+provide (quotient [dividend integer?] [divisor integer?])
  (binop \| (binop / dividend divisor) 0))

(define-checked+provide (even? [v integer?])
  (binop === (binop % v 2) 0))

(define-checked+provide (odd? [v integer?])
  (not (binop === (binop % v 2) 0)))

(define+provide (exact-nonnegative-integer? v)
  (and (#js.Number.isInteger v) (binop >= v 0)))

(define+provide (exact-integer? v)
  (#js.Number.isInteger v))

(define+provide (exact? v)
  (#js.Number.isInteger v))

(define+provide (single-flonum-available?) #f)

(define+provide *  (#js.Core.attachProcedureArity #js.Core.Number.mul 0))
(define+provide /  (#js.Core.attachProcedureArity #js.Core.Number.div 1))
(define+provide +  (#js.Core.attachProcedureArity #js.Core.Number.add 0))
(define+provide -  (#js.Core.attachProcedureArity #js.Core.Number.sub 1))
(define+provide <  (#js.Core.attachProcedureArity #js.Core.Number.lt 1))
(define+provide >  (#js.Core.attachProcedureArity #js.Core.Number.gt 1))
(define+provide <= (#js.Core.attachProcedureArity #js.Core.Number.lte 1))
(define+provide >= (#js.Core.attachProcedureArity #js.Core.Number.gte 1))
(define+provide =  (#js.Core.attachProcedureArity #js.Core.Number.equals 1))

(define-checked+provide (floor [v real?])
  (#js.Math.floor v))
(define-checked+provide (abs [v real?])
  (#js.Math.abs v))
(define-checked+provide (sin [v real?])
  (#js.Math.sin v))
(define-checked+provide (cos [v real?])
  (#js.Math.cos v))
(define-checked+provide (tan [v real?])
  (#js.Math.tan v))
(define-checked+provide (atan [v real?])
  (#js.Math.atan v))

(define-checked+provide (ceiling [v real?])
  (#js.Math.ceil v))
(define-checked+provide (round [v real?])
  (#js.Math.round v))

(define-checked+provide (min [a real?] [b real?])
  (#js.Math.min a b))
(define-checked+provide (max [a real?] [b real?])
  (#js.Math.max a b))

(define-checked+provide (log [v real?])
  (#js.Math.log v))

(define-checked+provide (expt [w number?] [z number?])
  (#js.Math.pow w z))

(define-checked+provide (sqrt [v number?])
  (#js.Math.sqrt v))

(define-checked+provide (sqr [v number?])
  (* v v))

(define-checked+provide (remainder [a integer?] [b integer?])
  (binop % a b))

(define-checked+provide (number->string [n number?])
  (#js.Core.UString.makeMutable (#js.n.toString)))

;; TODO: only works for numbers < 32 bits
(define-checked+provide (arithmetic-shift [n integer?] [m integer?])
  (if (negative? n)
      (binop >> n m)
      (binop << n m)))

;;TODO: Support bignums
(define+provide (inexact->exact x) x)
(define+provide (exact->inexact x) x)

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
(define-checked+provide (caddr [v (check/pair-of? #t (check/pair-of? #t pair?))])
  #js.v.tl.tl.hd)

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
  (#js.Core.Vector.makeInit size (or v 0)))

(define+provide vector? #js.Core.Vector.check)

(define-checked+provide (vector-length [v vector?])
  (#js.v.length))

(define-checked+provide (vector-ref [vec vector?] [i integer?])
  (#js.vec.ref i))

(define-checked+provide (vector-set! [vec vector] [i integer?] [v #t])
  (#js.vec.set i v))

(define-checked+provide (vector->list [vec vector?])
  (#js.Core.Pair.listFromArray #js.vec.items))

(define-checked+provide (vector->immutable-vector [vec vector?])
  (#js.Core.Vector.copy vec #f))

;; --------------------------------------------------------------------------
;; Hashes

(define-syntax-rule (make-hash-contructor make)
  (v-λ () #:unchecked
    (define kv* arguments)
    (when (binop !== (binop % #js.kv*.length 2) 0)
      (throw (#js.Core.racketContractError "invalid number of arguments")))
    (let ([items (array)])
      (loop+ [i 0 #js.kv*.length 2]
             (#js.items.push (array ($ kv* i) ($ kv* (+ i 1)))))
      (make items #f))))

(define+provide hash    (make-hash-contructor #js.Core.Hash.makeEqual))
(define+provide hasheqv (make-hash-contructor #js.Core.Hash.makeEqv))
(define+provide hasheq  (make-hash-contructor #js.Core.Hash.makeEq))

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
              "hash-set" "(and hash? immutable?)" 0 h k v))))

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
(define+provide sort9 #js.Kernel.sort9)
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

(define+provide symbol? #js.Core.Symbol.check)
(define+provide keyword? #js.Core.Keyword.check)

(define+provide (make-string k [c #\nul])
  (#js.Core.UString.repeatChar k c))

(define+provide (list->string lst)
  (#js.Kernel.listToString lst))

(define+provide (string->immutable-string [s string?])
  (#js.Core.UString.stringToImmutableString s))

(define-checked+provide (symbol->string [v symbol?])
  (#js.Core.UString.makeMutable (#js.v.toString)))

(define-checked+provide (string->symbol [s string?])
  (#js.Core.Symbol.make s))

(define-checked+provide (string->uninterned-symbol [s string?])
  (#js.Core.Symbol.makeUninterned s))

;; TODO: implement unreadable symbols
(define-checked+provide (string->unreadable-symbol [s string?])
  (#js.Core.Symbol.make s))

; Does not support prefixed forms such as "#b101".
(define+provide (string->number s [radix 10])
  (define (integer-in lo hi)
    (v-λ (v) #:unchecked
      (and (exact-integer? v) (>= v lo) (<= v hi))))
  (check/raise string? s 0)
  (check/raise (integer-in 2 16) radix 1)
  (let ([result (#js*.parseInt s radix)])
    (if (or (#js*.isNaN result)
            ; Work around parseInt permissiveness.
            (not (#js.s.isValidInteger radix)))
      #f
      result)))

(define-checked+provide (symbol-interned? [sym symbol?])
  ;;NOTE: We simply check if given symbol is equal to an
  ;; interned symbol.
  (binop === sym (#js.Core.Symbol.make #js.sym.v)))

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

(define+provide prop:procedure #js.Core.Struct.propProcedure)
(define+provide prop:equal+hash #js.Core.Struct.propEqualHash)

;; --------------------------------------------------------------------------
;; Errors

(define+provide error #js.Kernel.error)
(define+provide raise-argument-error #js.Kernel.argerror)
(define+provide raise-arguments-error #js.Kernel.argserror)
(define+provide raise-mismatch-error #js.Kernel.mismatcherror)

;; --------------------------------------------------------------------------
;; Bytes

(define+provide (bytes? bs)
  (#js.Core.Bytes.check bs))

(define-checked+provide (bytes->string/utf-8 [bs bytes?])
  (#js.Core.UString.fromBytesUtf8 bs))

(define-checked+provide (string->bytes/utf-8 [str string?])
  (#js.Core.UString.toBytesUtf8 str))

(define-checked+provide (bytes=? [bstr1 bytes?] [bstr2 bytes?])
  (#js.Core.Bytes.eq bstr1 bstr2))

(define-checked+provide (bytes<? [bstr1 bytes?] [bstr2 bytes?])
  (#js.Core.Bytes.lt bstr1 bstr2))

(define-checked+provide (bytes>? [bstr1 bytes?] [bstr2 bytes?])
  (#js.Core.Bytes.gt bstr1 bstr2))

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

(define+provide (procedure-arity-mask fn) (procedure-arity fn))
(define+provide (bitwise-bit-set? mask n) #t)
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

(define+provide (regexp-match pattern input)
  (#js.Core.Regexp.match pattern input))

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
      (#js.Core.Symbol.makeUninterned (binop + s __count)))))

(define+provide (eval-jit-enabled) #f)

(define+provide (variable-reference-constant? x) #f)
(define+provide (variable-reference-from-unsafe? x) #f)

(define+provide (inspector? p)
  #t)

(define+provide (make-thread-cell p) p)

(define __count 1000)

(define+provide system-type
  (v-λ (system-type mod) #:unchecked
    'javascript))

;; TODO: manually implement weak references? or ES6 WeakMap? see pr#106
(define+provide make-weak-hash make-hash)
(define+provide make-weak-hasheqv make-hasheqv)
(define+provide make-weak-hasheq make-hasheq)
