#lang racketscript/base

(require (for-syntax racketscript/base
                     syntax/parse))

(provide format-js-str
         generate-id
         js-string?
         js-object?
         null?
         undefined?
         js-array?
         msg->string)

(define-syntax-rule (format-js-str fmt-str args ...)
  (js-string (format fmt-str args ...)))


;; 
;; Funny words courtesy of ChatGPT
;; 

(define funny-adjectives (list "bumbling"
                               "quizzical"
                               "wacky"
                               "zany"
                               "fluffy"
                               "bizarre"
                               "hilarious"
                               "whimsical"
                               "absurd"
                               "goofy"
                               "ridiculous"
                               "loopy"
                               "nutty"
                               "eccentric"
                               "silly"
                               "quirky"
                               "jovial"
                               "giggly"
                               "mirthful"
                               "haphazard"
                               "chucklesome"
                               "fanciful"
                               "droll"
                               "boisterous"
                               "offbeat"
                               "hysterical"
                               "peculiar"
                               "lighthearted"
                               "playful"
                               "amusing"))

(define funny-nouns (list "goober"
                          "banana"
                          "sock-puppet"
                          "llama"
                          "rubber-chicken"
                          "pajamas"
                          "gobbledygook"
                          "poodle"
                          "bubble-wrap"
                          "tater-tot"
                          "cheeseburger"
                          "wiggle"
                          "snorkel"
                          "ticklemonster"
                          "jello"
                          "balloon-animal"
                          "slinky"
                          "spaghetti"
                          "bumblebee"
                          "dingleberry"
                          "flapdoodle"
                          "doohickey"
                          "noodle"
                          "gobbledygook"
                          "whatchamacallit"
                          "snickerdoodle"
                          "popsicle"
                          "gigglesnort"
                          "wobble"
                          "hootenanny"
                          "noodle"))

(define (generate-id)
  (define adjective (list-ref funny-adjectives (random (length funny-adjectives))))
  (define noun      (list-ref funny-nouns      (random (length funny-nouns))))
  (format "~a-~a" adjective noun))


(define (js-string? s)
  (or ($/typeof s "string") ($/instanceof s #js*.String)))

;; NOTE: because every racket datatype in 
;;       racketscript is stored as a js object,
;;       ($/typeof obj <racket variable>)
;;       will always be true
(define (js-object? obj)
  (and (not (string? obj)
            (number? obj)
            (boolean? obj)
            (list? obj)
            (symbol? obj)
            (struct? obj))
       ($/typeof obj "object")))

(define (null? val)
  ($/binop === val $/null))

(define (undefined? val)
  ($/binop === val $/undefined))

(define (js-array? arr)
  (#js*.Array.isArray arr))

(define (msg->string msg)
  (cond [(undefined? msg) "undefined"]
        [(js-string? msg) (js-string->string msg)]
        [(or (js-object? msg) (js-array? msg) (null? msg))
         (#js*.JSON.stringify msg)]
        [else (format "~a" msg)]))

