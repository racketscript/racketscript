#lang typed/racket/base

(require racket/bool
         racket/format
         racket/match
         racket/set
         typed/rackunit
         "config.rkt")

(require/typed racket/string
  [string-prefix? (-> String String Boolean)])

(provide fresh-id
         fresh-id-counter
         reserved-keyword?
         normalize-symbol)

;;; Identifier renaming -------------------------------------------------------

(: normalize-symbol (->* (Symbol) ((Listof String)) String))
;;; NOTE: Just normalizing is still not a safe way to translate to JS.
(define (normalize-symbol s [ignores '()])
  ;; TODO: handle every weird character in symbol
  ;; Since every identifier is suffixed with fresh symbol
  ;; we don't have to worry about name clashes after this
  ;; naive renaming.
  (: should-rename? (-> String Boolean))
  (define (should-rename? s)
    (not (string-prefix? s (jsruntime-core-module))))

  (define ss (symbol->string s))

  (cond
    [(false? (should-rename? ss)) ss]
    [(reserved-keyword? s)
     (~a "r" ss)]
    [else

     (match-define (cons ch-first ch-rest) (string->list ss))
      (apply string-append
             (cons (normalize-symbol-atom ch-first #t ignores)
                   (let loop : (Listof String) ([ss : (Listof Char) ch-rest])
                        (match ss
                          [(list) null]
                          [(list* "-" ">" rst) (cons "_to_" (loop rst))]
                          [(list* c rst) (cons (normalize-symbol-atom c #f ignores) (loop rst))]))))]))

(module+ test
  (check-equal? (normalize-symbol '7am) "_7am")
  (check-equal? (normalize-symbol 'foobar) "foobar")
  (check-equal? (normalize-symbol '+) "__plus_")
  (check-equal? (normalize-symbol 'hello-world) "hello_world")
  (check-equal? (normalize-symbol 'document.write+print (list "." "+"))
                "document.write+print"
                "characters in ignores parameter is not replaced")
  (check-equal? (normalize-symbol 'document.write (list ".")) "document.write"
                "characters in ignores parameter is not replaced"))

(: valid-literal-atom? (-> Char Boolean Boolean))
(define (valid-literal-atom? ch first?)
  (define patt
    ;;TODO: Not entirely sure about the second case. Check specs again.
    (if first?
        #px"\\$|_|\\p{L}|\\p{Nl}"
        #px"\\$|_|\\p{L}|\\p{Nl}|\\p{Mn}|\\p{Mc}|\\p{Nd}|\\p{Pc}"))
  (regexp-match-exact? patt (string ch)))
(module+ test
  (check-true (valid-literal-atom? #\a #f))
  (check-true (valid-literal-atom? #\$ #f))
  (check-true (valid-literal-atom? #\_ #f))
  (check-true (valid-literal-atom? #\3 #f))
  (check-false (valid-literal-atom? #\3 #t)))

(: reserved-keyword? (-> Symbol Boolean))
(define (reserved-keyword? s)
  (set-member? es6-reserved-keywords s))

(: normalize-symbol-atom (->* (Char Boolean) ((Listof String)) String))
(define (normalize-symbol-atom ch first? [ignores '()])
  (: char-map (HashTable String  String))
  (define char-map
    #hash(("-" . "_")
          ("?" . "_p")
          ("+" . "_plus_")
          ("'" . "_prime_")
          ("*" . "_times_")
          ("/" . "_by_")
          ("=" . "_eq_")
          ("<" . "_lt_")
          (">" . "_gt_")
          ("!" . "_bang_")
          ("." . "_dot_")
          ("&" . "_and_")))

  (define sch (string ch))

  (cond
    [(or (member sch ignores)
         (valid-literal-atom? ch first?))
     sch]
    [first? ;; first atom is not valid
     (string-append "_" (normalize-symbol-atom ch #f))]
    [(hash-has-key? char-map sch)
     (hash-ref char-map sch)]
    [else "_"]))
(module+ test
  (check-true #t))

;;;----------------------------------------------------------------------------
(: fresh-id-counter (Parameter Nonnegative-Integer))
;; Used when test-environment? is true.
(define fresh-id-counter (make-parameter 0))

(: fresh-id (-> Symbol Symbol))
(define fresh-id
  (if (test-environment?)
      gensym
      (λ (id)
        (fresh-id-counter (add1 (fresh-id-counter)))
        (string->symbol (~a id (fresh-id-counter))))))
(module+ test
  (check-equal?
   (parameterize ([test-environment? #t]
                  [fresh-id-counter 0])
     (list (fresh-id 'foo)
           (fresh-id-counter)))
   (list 'foo1 1)
   "fresh-id counter should get incremented"))

;;-----------------------------------------------------------------------------
(: es6-reserved-keywords (Setof Symbol))
(define es6-reserved-keywords
  (list->set
   '(abstract
     arguments
     await*
     boolean
     break
     byte
     case
     catch
     char
     class*
     const
     continue
     debugger
     default
     delete
     do
     double
     else
     enum*
     eval
     export*
     extends*
     false
     final
     finally
     float
     for
     function
     goto
     if
     implements
     import*
     in
     instanceof
     int
     interface
     let*
     long
     native
     new
     null
     package
     private
     protected
     public
     return
     short
     static
     super*
     switch
     synchronized
     this
     throw
     throws
     transient
     true
     try
     typeof
     var
     void
     volatile
     while
     with
     yield)))
