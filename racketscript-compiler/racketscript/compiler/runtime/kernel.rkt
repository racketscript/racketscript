#lang racketscript/boot

(require racketscript/interop
         racket/stxparam
         (for-syntax syntax/parse))

;; ----------------------------------------------------------------------------
;; JS imports

(define Kernel ($/require "./kernel.js")) ;; old stuff
(define Core   ($/require "./core.js"))
(define Paramz ($/require "./paramz.js"))
(define Values #js.Core.Values)
(define Pair   #js.Core.Pair)

;; ----------------------------------------------------------------------------
;; Helpers

(define-syntax throw   (make-rename-transformer #'$/throw))
(define-syntax new     (make-rename-transformer #'$/new))
(define-syntax array   (make-rename-transformer #'$/array))
(define-syntax object  (make-rename-transformer #'$/obj))
(define-syntax :=      (make-rename-transformer #'$/:=))
(define-syntax binop   (make-rename-transformer #'$/binop))
(define-syntax typeof  (make-rename-transformer #'$/typeof))
(define-syntax instanceof  (make-rename-transformer #'$/instanceof))

(define-syntax *null*       (make-rename-transformer #'$/null))
(define-syntax *undefined*  (make-rename-transformer #'$/undefined))

(define-syntax define-binop
  (syntax-parser
    [(_ name:id oper:id)
     #`(define-syntax name
         (syntax-parser
           [(op e0:expr e1:expr) #`(binop oper e0 e1)]
           [(op e0:expr e1:expr en:expr ...+)
            #'(op (binop oper e0 e1) en (... ...))]))]))

(define-binop and &&)
(define-binop or \|\|)

(define-syntax introduce-id
  (syntax-parser
    [(_ id:id) #'(define-syntax id
                   (syntax-parser
                     [(_ e0 (... ...)) #'(($ 'id) e0 (... ...))]
                     [_:id #'($ 'id)]))]))

(define-syntax (define+provide stx)
  (syntax-parse stx
    [(_ name:id val:expr)
     #'(begin (provide name)
              (define name val))]
    [(_ (~and formals (name:id . args)) body ...)
     #'(begin (provide name)
              (define formals body ...))]))

(define-syntax-parameter arguments
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used in JS vararg lambda")))

;; v-λ is a lambda with JS semantics. More specifically, for rest
;; parameters are plain arrays instead of Racket lists. For sake of
;; performance, v-λ is preferred for writing variadic functions in
;; this file. `arguments` is syntax parameter to get the native JS
;; arguments object.
;;
;; NOTE: Use `arguments` directly with extreme care! Loops are
;; lambdas, therefore, rebind this variable with different name.
(define-syntax (v-λ stx)
  (define (-arguments stx)
    (syntax-parse stx
      [(_ i:expr) #'($ 'arguments i)]
      [(_ i:expr j:expr) #'($ 'arguments i)]
      [arguments #'($ 'arguments)]))
  (syntax-parse stx
    [(_ args:id body ...+)
     #`(syntax-parameterize ([arguments #,-arguments])
         (λ ()
           (define args (#js.Core.argumentsToArray arguments))
           body ...))]
    [(_ (a0:id ...) body ...+)
     #`(syntax-parameterize ([arguments #,-arguments])
         (λ (a0 ...)
           body ...))]
    [(_ (a0:id ...+ . as:id) body ...+)
     (define fixed-args (length (syntax-e #'(a0 ...))))
     #`(syntax-parameterize ([arguments #,-arguments])
         (λ (a0 ...)
           (define as (#js.Array.prototype.slice.call arguments #,fixed-args))
           body ...))]))

(define-syntax (loop+ stx)
  (syntax-parse stx
    [(_ [index:id start:expr end:expr step:expr] body ...+)
     #`(let loop ([index start])
         (when (binop < index end)
           body ...
           (loop (binop + index step))))]
    [(_ [index:id start:expr end:expr] body ...+)
     #'(loop+ [index start end 1] body ...)]
    [(_ [index:id end:expr] body ...+)
     #'(loop+ [index 0 end 1] body ...)]))

(define-syntax (for/array stx)
  (syntax-parse stx
    [(_ [(index:id item:id) arr:expr start:expr end:expr] body ...+)
     ;; TODO: save the arr than than copying it everywhere
     #'(loop+ [index start end 1]
         (define item ($ arr index))
          body ...)]
    [(_ [(index:id item:id) arr:expr] body ...+)
     #'(for/array [(index item) arr 0 ($ arr 'length)] body ...)]
    [(_ [item:id arr:expr start:expr end:expr] body ...+)
     #'(for/array [(i item) arr start end] body ...)]
    [(_ [item:id arr:expr start:expr] body ...+)
     #'(for/array [item arr start ($ arr 'length)] body ...)]
    [(_ [item:id arr:expr] body ...+)
     #'(for/array [item arr 0 ($ arr 'length)] body ...)]))

;; Use some Native JS libs

(introduce-id Math)
(introduce-id Number)
(introduce-id String)
(introduce-id Uint8Array)
(introduce-id Date)
(introduce-id Array)
(introduce-id console)

;; ----------------------------------------------------------------------------
;; Errors

(define-syntax-rule (type-check/raise type what)
  (unless (#js.type.check what)
    (throw (#js.Core.racketContractError "expected a {0}, but given {1}"
                                         type
                                         what))))

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

(define+provide number? #js.Core.Number.check)
(define+provide real? #js.Core.Number.check)
(define+provide integer? #js.Core.Number.isInteger)

(define+provide (zero? v)
  (binop === v 0))

(define+provide (positive? v)
  (binop > v 0))

(define+provide (negative? v)
  (binop < v 0))

(define+provide (add1 v)
  (binop + v 1))

(define+provide (sub1 v)
  (binop - v 1))

(define+provide (quotient dividend divisor)
  (floor (binop / dividend divisor)))

(define+provide (even? v)
  (binop === (binop % v 2) 0))

(define+provide (odd? v)
  (not (binop === (binop % v 2) 0)))

(define+provide (exact-nonnegative-integer? v)
  (and (#js.Number.isInteger v) (binop >= v 0)))

(define+provide (exact->inexact v) v)

(define+provide * #js.Core.Number.mul)
(define+provide / #js.Core.Number.div)
(define+provide + #js.Core.Number.add)
(define+provide - #js.Core.Number.sub)
(define+provide < #js.Core.Number.lt)
(define+provide > #js.Core.Number.gt)
(define+provide <= #js.Core.Number.lte)
(define+provide >= #js.Core.Number.gte)
(define+provide = #js.Core.Number.equals)

(define+provide floor #js.Math.floor)
(define+provide abs #js.Math.abs)
(define+provide sin #js.Math.sin)
(define+provide cos #js.Math.cos)
(define+provide tan #js.Math.tan)
(define+provide ceiling #js.Math.ceiling)
(define+provide round #js.Math.round)
(define+provide min #js.Math.min)
(define+provide max #js.Math.max)

;;TODO: Support bignums
(define+provide (expt w z) (#js.Math.pow w z))
(define+provide (sqrt v) (#js.Math.sqrt v))

(define+provide (sqr v)
  (* v v))

(define+provide (remainder a b)
  (binop % a b))

(define+provide (number->string n)
  (#js.n.toString))

;; ----------------------------------------------------------------------------
;; Booleans

(define+provide (not v)
  (or (equal? v #f) #f))

(define+provide false #f)
(define+provide true #t)

(define+provide (false? v) (eq? v #f))

;; ----------------------------------------------------------------------------
;; Pairs

(define+provide (car pair) #js.pair.hd)
(define+provide (cdr pair) #js.pair.tl)
(define+provide cons #js.Pair.make)
(define+provide cons? #js.Pair.check)
(define+provide pair? #js.Pair.check)

(define+provide empty #js.Pair.Empty)
(define+provide null #js.Pair.Empty)
(define+provide list #js.Pair.makeList)
(define+provide first car)
(define+provide rest  cdr)

(define+provide null?  #js.Pair.isEmpty)
(define+provide empty? #js.Pair.isEmpty)
(define+provide length #js.Pair.listLength)

(define+provide (second lst)
  ($> lst (cdr) (car)))

(define+provide (list? v)
  (cond
    [(null? v) #t]
    [(cons? v) (list? ($> v (cdr)))]
    [else #f]))

(define+provide (reverse lst)
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
    (#js.map.apply *null* ($> (array lam) (concat lsts)))
    *null*))

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

;; --------------------------------------------------------------------------
;; Vectors

(define+provide vector
  (v-λ ()
     (#js.Core.Vector.make (#js.Core.argumentsToArray arguments) #t)))

;; v is optional
(define+provide (make-vector size v)
  (#js.Core.Vector.makeInit size (or v 0)))

(define+provide vector? #js.Core.Vector.check)

(define+provide (vector-length v)
  (#js.v.length))

;; TODO: wrong checks
(define+provide (vector-ref vec i)
  (#js.Core.Vector.check vec)
  (#js.vec.ref i))

(define+provide (vector-set! vec i v)
  (#js.Core.Vector.check vec)
  (#js.vec.set i v))

(define+provide (vector->list vec)
  (#js.Core.Pair.listFromArray #js.vec.items))

;; --------------------------------------------------------------------------
;; Hashes

(define+provide (make-immutable-hash assocs)
  (#js.Core.Hash.makeFromAssocs assocs "equal" #f))

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
  (#js.Core.Hash.makeFromAssocs assocs "equal" #t))
(define+provide (make-hasheqv assocs)
  (#js.Core.Hash.makeFromAssocs assocs "eqv" #t))
(define+provide (make-hasheq assocs)
  (#js.Core.Hash.makeFromAssocs assocs "eq" #t))

(define+provide (hash-ref h k fail)
  (#js.h.ref k fail))

(define+provide (hash-set h k v)
  (#js.h.set k v))

;; --------------------------------------------------------------------------
;; Higher Order Functions

(define+provide apply
  (v-λ (lam . args)
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

(provide prop:evt evt?)
(define-values (prop:evt evt?) (#js.Core.Struct.makeStructTypeProperty
                                {object [name "prop:evt"]}))

;; --------------------------------------------------------------------------
;; Ports

(define+provide (current-output-port) #f)
(define+provide (output-port?) #f)

;; --------------------------------------------------------------------------
;; Printing

(define+provide (displayln v) (#js.Kernel.displayln v))
(define+provide (display v) (#js.Kernel.display v))

(define+provide (newline)
  (displayln ""))

(define+provide (print-values v)
  (unless (void? v)
    (if (typeof v "string")
        (#js.console.log (string "\"" v "\"")) ;;HACK: special cases
        (displayln v))))

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

(define+provide current-continuation-marks #js.Core.Marks.getFrames)

(define+provide (continuation-mark-set->list mark-set key)
  (#js.Core.Marks.getMarks mark-set key))

(define+provide (continuation-mark-set-first mark-set key-v none-v prompt-tag)
  ;; TODO: implement prompt tag
  (define mark-set (or mark-set (#js.Core.Marks.getFrames)))
  (define marks (#js.Core.Marks.getMarks mark-set key-v))
  (if (null? marks)
      none-v
      #js.marks.hd))

(define+provide make-parameter #js.Paramz.makeParameter)

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

(define+provide regexp? #js.Kernel.isRegExp)
(define+provide pregexp? #js.Kernel.isPRegExp)
(define+provide byte-regexp? #js.Kernel.isByteRegExp)
(define+provide byte-pregexp? #js.Kernel.isBytePRegExp)
(define+provide regexp #js.Kernel.regexp)
(define+provide pregexp #js.Kernel.pregexp)
(define+provide byte-regexp #js.Kernel.byteRegExp)
(define+provide byte-pregexp #js.Kernel.bytePRegExp)
(define+provide regexp-match #js.Kernel.regexpMatch)

;; --------------------------------------------------------------------------
;; Procedures

(define+provide (procedure? f)
  (typeof f "function"))

(define+provide (procedure-arity-includes? f) #t)

(define+provide (procedure-arity f)
  #js.f.length)

(define+provide (eval-jit-enabled) #t)

(define+provide (make-sequence who v)
  (#js.Core.Values.make [array car cdr v pair? #f #f]))

(define+provide (variable-reference-constant? x) #f)
