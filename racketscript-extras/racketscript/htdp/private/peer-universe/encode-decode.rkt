#lang racketscript/base

(provide encode-data
         decode-data)

(require "util.rkt")

(define DATA-TYPE-WARNING #js"racketscript/htdp/universe: Unsupported datatype being passed to/from server.")

(define (encode-array arr)
  (#js.arr.map (lambda (elem) (encode-data elem))))

(define (decode-array arr)
  (#js.arr.map (lambda (elem) (decode-data elem))))

(define (encode-object obj)
  (define keys (#js*.Object.keys obj))
  (#js.keys.reduce (lambda (res key)
                     ($/:= ($ res key) (encode-data ($ obj key)))
                     res)
                   ($/obj)))

(define (decode-object obj)
  (define keys (#js*.Object.keys obj))
  (#js.keys.reduce (lambda (res key)
                                 ($/:= ($ res key) (decode-data ($ obj key)))
                                 res)
                               ($/obj)))

#|
('test "some_string" #js"test" {test: "test"})


"test"
{
  val: "test", type: "string"
}

'sym
{
  val: "sym", type: "symbol"
}

|#

(define (encode-data data)
  (cond [(list? data) (foldl (lambda (curr result)
                               (#js.result.push (encode-data curr))
                               result)
                      ($/array)
                      data)]
        [(null? data)      ($/obj [type #js"null"])]
        [(undefined? data) ($/obj [type #js"undefined"])]    
        [(number? data)    ($/obj [type #js"number"]
                                  [val data])]
        [(string? data)    ($/obj [type #js"string"]
                                  [val (js-string data)])]
        [(symbol? data)    ($/obj [type #js"symbol"]
                                  [val (js-string (symbol->string data))])]
        [(boolean? data)   ($/obj [type #js"boolean"]
                                  [val data])]
        [(js-string? data) ($/obj [type #js"js-string"]
                                  [val data])]
        [(js-array? data)  ($/obj [type #js"js-array"]
                                  [val (encode-array data)])]
        [(js-object? data) ($/obj [type #js"js-object"]
                                  [val (encode-object data)])]
        [else              (begin 
                             (#js*.console.warn ($/array DATA-TYPE-WARNING data))
                             ($/obj [type #js"unknown"]
                                  [val data]))]))

(define (decode-data data)
  (cond [(#js*.Array.isArray data) (#js.data.reduce (lambda (result curr)
                                                      (append result (list (decode-data curr))))
                                                    '())]
        [($/binop == #js.data.type #js"null") $/null]
        [($/binop == #js.data.type #js"undefined") $/undefined]
        [($/binop == #js.data.type #js"number")    #js.data.val]
        [($/binop == #js.data.type #js"string")    (js-string->string #js.data.val)]
        [($/binop == #js.data.type #js"symbol")    (string->symbol (js-string->string #js.data.val))]
        [($/binop == #js.data.type #js"boolean")   #js.data.val]
        [($/binop == #js.data.type #js"js-string") #js.data.val]
        [($/binop == #js.data.type #js"js-array")  (decode-array #js.data.val)]
        [($/binop == #js.data.type #js"js-object") (decode-object #js.data.val)]
        [($/binop == #js.data.type #js"unknown")   (begin
                                                     (#js*.console.warn DATA-TYPE-WARNING)
                                                     #js.data.val)]))