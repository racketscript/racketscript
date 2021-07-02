#lang racketscript/base

(require racketscript/interop
         racket/string
         (for-syntax racket/base syntax/parse racket/syntax))

(define-syntax define-js-checker
  (syntax-parser
    [(_ name #:checks-js thing)
     #:with thing-pred? (format-id #'thing "js-~a?" #'thing)
     #'(define-syntax name
         (syntax-parser
           [(_ e) #'(name e "")]
           [(_ e label)
            #'(let ([x e])
                (when (non-empty-string? label)
                  (printf "~a " label))
                (printf "~v is a js ~a?: ~a\n" x 'thing (thing-pred? x)))]))]))

(define-js-checker check-js-obj #:checks-js object)
(define-js-checker check-js-array #:checks-js array)

(define-syntax check-js-objs+arrays
  (syntax-parser
    [(_) #'(begin)]
    [(_ e #:label str . rst)
     #'(begin
         (let ([x e])
           (check-js-obj x str)
           (check-js-array x str))
         (check-js-objs+arrays . rst))]
    [(_ e . rst) ; no label
     #'(begin
         (let ([x e])
           (check-js-obj x)
           (check-js-array x))
         (check-js-objs+arrays . rst))]))

(check-js-objs+arrays {$/obj}
                      [$/array 1 2 3] #:label "raw js array"
                      (make-hash)
                      (vector 1 2 3)
                      #"ABC" #:label "racket bytestring" ; Uint8Array TypedArray is not Array
                      #\D #:label "racket char"
                      "a racket string"
                      #js"a js string"
                      '()
                      '(1 2 3)
                      1
                      #f
                      $/null)

