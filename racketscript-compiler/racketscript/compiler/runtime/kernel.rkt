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
  (v-λ vals
    (if (binop === #js.vals.length 1)
        ($ vals 0)
        (#js.Values.make vals))))

(define+provide (call-with-values generator receiver)
  (let ([vals (generator)])
    (cond
      [(#js.Values.check vals)
       (#js.receiver.apply #js*.this (#js.vals.getAll))]
      [(not (or (eq? vals *undefined*) (eq? vals *null*)))
       (#js.receiver.apply #js*.this (array vals))])))

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

(define+provide *  #js.Core.Number.mul)
(define+provide /  #js.Core.Number.div)
(define+provide +  #js.Core.Number.add)
(define+provide -  #js.Core.Number.sub)
(define+provide <  #js.Core.Number.lt)
(define+provide >  #js.Core.Number.gt)
(define+provide <= #js.Core.Number.lte)
(define+provide >= #js.Core.Number.gte)
(define+provide =  #js.Core.Number.equals)

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
  (#js.n.toString))

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

;; ----------------------------------------------------------------------------
;; Pairs

(define-checked+provide (car [pair pair?]) #js.pair.hd)
(define-checked+provide (cdr [pair pair?]) #js.pair.tl)
(define+provide cons       #js.Pair.make)
(define+provide cons?      #js.Pair.check)
(define+provide pair?      #js.Pair.check)

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

(define+provide null  #js.Pair.Empty)
(define+provide list  #js.Pair.makeList)

(define+provide null?  #js.Pair.isEmpty)
(define+provide empty? #js.Pair.isEmpty)
(define+provide length #js.Pair.listLength)

(define+provide (list? v)
  (cond
    [(null? v) #t]
    [(cons? v) (list? ($> v (cdr)))]
    [else #f]))

(define-checked+provide (reverse [lst list?])
  (let loop ([lst lst]
             [result '()])
    (if (null? lst)
        result
        (loop #js.lst.tl (#js.Core.Pair.make #js.lst.hd result)))))

(define+provide (list* a0 . args)
  (define lst (reverse (cons a0 args)))
  (let loop ([rst (cdr lst)]
             [result (car lst)])
    (if (null? rst)
        rst
        (loop (cdr rst)
              (cons (car rst) result)))))

(define+provide append
  (v-λ ()
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

(define+provide (make-struct-type name
                                  super-type
                                  init-field-count
                                  auto-field-count
                                  auto-v
                                  props
                                  inspector
                                  proc-spec
                                  immutables
                                  guard
                                  constructor-name)
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
           [constructorName constructor-name]}))

(define+provide (make-struct-field-accessor ref index field-name)
  (λ (s)
    (ref s index)))

(define+provide (make-struct-field-mutator set index fieldName)
  (λ (s v)
    (set s index v)))

(define+provide (make-struct-type-property name guard supers can-impersonate?)
  (#js.Core.Struct.makeStructTypeProperty
   {object [name name]
           [guard guard]
           [supers supers]
           [canImpersonate can-impersonate?]}))

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
  (v-λ ()
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
  (v-λ ()
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

(define+provide (make-hash assocs)
  (define assocs* (or assocs '()))
  (#js.Core.Hash.makeFromAssocs assocs* "equal" #t))
(define+provide (make-hasheqv assocs)
  (define assocs* (or assocs '()))
  (#js.Core.Hash.makeFromAssocs assocs* "eqv" #t))
(define+provide (make-hasheq assocs)
  (define assocs* (or assocs '()))
  (#js.Core.Hash.makeFromAssocs assocs* "eq" #t))

(define+provide (make-immutable-hash assocs)
  (define assocs* (or assocs '()))
  (#js.Core.Hash.makeFromAssocs assocs* "equal" #f))
(define+provide (make-immutable-hasheqv assocs)
  (define assocs* (or assocs '()))
  (#js.Core.Hash.makeFromAssocs assocs* "eqv" #f))
(define+provide (make-immutable-hasheq assocs)
  (define assocs* (or assocs '()))
  (#js.Core.Hash.makeFromAssocs assocs* "eq" #f))

(define+provide (hash-ref h k fail)
  (#js.h.ref k fail))

(define+provide (hash-set h k v)
  (#js.h.set k v))

(define+provide (hash-set! h k v)
  (#js.h.set k v))

(define+provide (hash-map h proc)
  (#js.Core.Hash.map h proc))

;; --------------------------------------------------------------------------
;; Higher Order Functions

(define+provide apply
  (v-λ (lam . args)
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
    (#js.foldl.apply #js*.this
                     ($> (array (v-λ args
                                  (define final-arg (#js.args.pop))
                                  (and (or final-arg
                                           (#js.fn.apply *null* args))
                                       #t))
                                #f)
                         (concat lists)))))

(define+provide andmap
  (v-λ (fn . lists)
    (#js.foldl.apply #js*.this
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
    (v-λ ()
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
    (λ (v)
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

;; TODO: support both mutable/immutable strings

(define+provide string (#js.String.prototype.concat.bind ""))

(define+provide ~a
  (v-λ args
    ($> (array)
        reduce
        (call args
              (λ (x r)
                (binop + r (#js.Core.toString x)))
              ""))))

(define+provide string-append string)

(define+provide (string=? sa sb)
  (binop === sa sb))

(define+provide (string<? sa sb)
  (binop < sa sb))

(define+provide (string<=? sa sb)
  (binop <= sa sb))

(define+provide (string>? sa sb)
  (binop > sa sb))

(define+provide (string>=? sa sb)
  (binop >= sa sb))

(define+provide (string? v)
  (typeof v "string"))

(define+provide format #js.Kernel.format)
(define+provide symbol? #js.Core.Symbol.check)

(define+provide (symbol->string v)
  (#js.v.toString))

(define+provide (symbol=? s v)
  (#js.s.equals v))

(define+provide (string-length v)
  #js.v.length)

(define+provide (string-downcase v)
  (#js.v.toLowerCase v))

(define+provide (string-upcase v)
  (#js.v.toUpperCase v))

;; end is optional
(define+provide (substring str start end)
  (define end (or end #f))
  (cond
    [(not (typeof str "string"))
     (throw (#js.Core.racketContractError "expected a string"))]
    [(binop < start 0)
     (throw (#js.Core.racketContractError "invalid start index"))]
    [(and (binop !== end #f)
               (or (binop < end 0) (binop > end #js.str.length)))
     (throw (#js.Core.racketContractError "invalid end index"))]
    [(binop === end #f)
     (set! end #js.str.length)])
  (#js.str.substring start end))

(define+provide (string-split str sep)
  (#js.Core.Pair.listFromArray (#js.str.split sep)))

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

;; --------------------------------------------------------------------------
;; Ports + Writers

(define+provide (current-output-port)
  #js.Core.Ports.standardOutputPort)

(define+provide (current-print)
  (λ (p)
    (when (string? p)
      (display "\""))
    (display p)
    (when (string? p)
      (display "\""))
    (newline)))

(define+provide (input-port? p)
  (#js.Core.Ports.checkInputPort p))

(define+provide (output-port? p)
  (#js.Core.Ports.checkOutputPort p))

;; --------------------------------------------------------------------------
;; Printing

(define+provide (display v) (#js.Kernel.display v))

(define+provide (newline)
  (display "\n"))

;; --------------------------------------------------------------------------
;; Errors

(define+provide error #js.Kernel.error)

;; --------------------------------------------------------------------------
;; Bytes

(define+provide (bytes? bs)
  (instanceof bs Uint8Array))

(define+provide (bytes->string/utf-8 bs)
  (if (bytes? bs)
      (#js.String.fromCharCode.apply *null* bs)
      (throw (#js.Core.racketContractError "expected bytes"))))

(define+provide (string->bytes/utf-8 str)
  (if (typeof str "string")
      (new (Uint8Array (#js.Array.prototype.map.call str
                                                     (λ (x) (#js.x.charCodeAt 0)))))
      (throw (#js.Core.racketContractError "expected string"))))

;; --------------------------------------------------------------------------
;; Continuation Marks

(define+provide current-continuation-marks   #js.Core.Marks.getContinuationMarks)
(define+provide continuation-mark-set->list  #js.Core.Marks.getMarks)

(define+provide (continuation-mark-set-first mark-set key-v none-v prompt-tag)
  ;; TODO: implement prompt tag
  (define mark-set (or mark-set (#js.Core.Marks.getContinuationMarks prompt-tag)))
  (define marks (#js.Core.Marks.getMarks mark-set key-v prompt-tag))
  (if (null? marks)
      none-v
      #js.marks.hd))

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

(define+provide (raise e)
  (let ([abort-ccp (continuation-mark-set-first (current-continuation-marks)
                                                #js.Paramz.ExceptionHandlerKey)])
    (abort-ccp e)))

;; --------------------------------------------------------------------------
;; Not implemented/Unorganized/Dummies

(define+provide (current-inspector) #t)
(define+provide raise-argument-error error)
(define+provide (check-method) #f)

(define+provide random #js.Kernel.random)

(define+provide (current-seconds)
  (#js.Math.floor (binop / (#js.Date.now) 1000)))

;; --------------------------------------------------------------------------
;; Regexp

;; TODO: both regexps and pregexps currently compile to js regexps,
;;       but js doesnt support posix patterns

(define+provide (regexp? v)
  (instanceof v RegExp))

(define+provide pregexp? regexp?)
(define+provide byte-regexp? regexp?)
(define+provide byte-pregexp? regexp?)

(define+provide (regexp str)
  (if (typeof str "string")
      (throw (#js.Core.racketContractError "expected string"))
      (new (RegExp str))))

(define+provide pregexp regexp)

(define+provide (byte-regexp bs)
  (if (bytes? bs)
      (new (RegExp (bytes->string/utf-8 bs)))
      (throw (#js.Core.racketContractError "expected bytes"))))

(define+provide byte-pregexp byte-regexp)

(define+provide (regexp-match p i)
  (define rx-p? (regexp? p))
  (define bytes-p? (bytes? p))
  (define bytes-i? (bytes? i))
  (define str-p? (binop === (typeof p) "string"))
  (define str-i? (binop === (typeof i) "string"))

  (when (and (not (or rx-p? bytes-p? str-p?))
             (not (or bytes-i? str-i?)))
    (throw
     (#js.Core.racketContractError
      "expected regexp, string or byte pat, and string or byte input")))

  (define str (if str-i? i (bytes->string/utf-8 i)))
  (define pat (cond
                [rx-p? p]
                [str-p? p]
                [else (bytes->string/utf-8 p)]))
  (define res (#js.str.match pat))
  (cond
    [(binop === res *null*) #f]
    [(and (or str-p? rx-p?) str-i?)
     (#js.Core.Pair.listFromArray
      (#js.res.map (λ (x)
                     (if (binop === x *undefined*)
                         #f
                         x))))]
    [else
     (#js.Core.Pair.listFromArray
      (#js.res.map (λ (x)
                     (if (binop === x *undefined*)
                         #f
                         (string->bytes/utf-8 x)))))]))

;; --------------------------------------------------------------------------
;; Procedures

;;TODO: Why was this prefixed with 'kernel:'???
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

(define+provide (procedure-arity-includes? f) #t)

(define+provide (procedure-arity fn)
  (define lambda-type #js.fn.__rjs_lambdaType)
  (cond
    [(binop === lambda-type "variadic")
     (kernel:arity-at-least (or #js.fn.__rjs_arityValue #js.fn.length))]
    [(binop === lambda-type "case-lambda")
     (if (binop === #js.fn.__rjs_arityValue.length 1)
         ($ #js.fn.__rjs_arityValue 0)
         (#js.Core.Pair.listFromArray #js.fn.__rjs_arityValue))]
    [else #js.fn.length]))

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

(define+provide (gensym sym)
  (let ([s (or (and sym #js.sym.v) "")])
    (set! __count (binop + __count 1))
    (#js.Core.Symbol.makeUninterned (binop + s __count))))

(define+provide (eval-jit-enabled) #f)

(define+provide (variable-reference-constant? x) #f)

(define+provide (inspector? p)
  #t)

(define+provide (make-thread-cell p) p)

(define __count 1000)

(define+provide (system-type mod)
  'javascript)

(define+provide make-weak-hash make-hash)
