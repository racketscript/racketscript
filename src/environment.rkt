#lang typed/racket/base

(require racket/format
         racket/match
         "config.rkt"
         "util.rkt")

(provide name-in-module
         module-output-file
         BASE-ENV)

(define-type-alias RenameMap (HashTable Symbol Symbol))

(: name-in-module (-> Symbol Symbol Symbol))
(define (name-in-module mod name)
  (match mod
    ['core (string->symbol (~a (jsruntime-core-module) "." name))]
    ['kernel (string->symbol (~a (jsruntime-kernel-module) "." name))]
    [_ (error "Invalid module name")]))

(: add-import (-> RenameMap String Symbol RenameMap))
(define (add-import env prefix id)
  (hash-set env id (string->symbol (~a prefix "." (normalize-symbol id)))))

(: add-imports* (-> RenameMap String (Listof Symbol) RenameMap))
(define (add-imports* env prefix ids*)
  (foldl (λ ([id : Symbol] [env* : RenameMap])
           (add-import env* prefix id))
         env
         ids*))

(: add-kernel-imports* (-> RenameMap (Listof Symbol) RenameMap))
(define (add-kernel-imports* env ids*)
  (add-imports* env (jsruntime-kernel-module) ids*))

(: add-core-imports* (-> RenameMap (Listof Symbol) RenameMap))
(define (add-core-imports* env ids*)
  (add-imports* env (jsruntime-core-module) ids*))

(: CORE-IMPORTS RenameMap)
(define CORE-IMPORTS
  ;; TODO: Many of these could be represented using JS operators
  (let ([build-core-env (λ ([pairs* : (Listof (Pairof Symbol Symbol))])
                          (map (λ (p*)
                                 (match-define (cons k v) p*)
                                 (cons k (string->symbol (~a (jsruntime-core-module) "." v))))
                               pairs*))])
    ((inst make-immutable-hash Symbol Symbol)
     (build-core-env
      '((* . Number.mul)
        (+ . Number.add)
        (- . Number.sub)
        (/ . Number.div)
        (< . Number.lt)
        (> . Number.gt)
        (<= . Number.lte)
        (>= . Number.gte)
        (= . Number.equal))))))
  
(: BASE-ENV (HashTable Symbol Symbol))
(define BASE-ENV
  (add-kernel-imports*
   CORE-IMPORTS
   '(zero?
     car
     cdr
     list
     first
     rest
     sub1
     add1
     displayln
     equal?
     values
     call-with-values
     not
     empty?
     print-values
     cons
     null?
     empty?
     list?

     ~a
     string-append
     display
     )))

(: module-output-file (-> (U String Symbol) Path))
(define (module-output-file mod)
  (cond
    [(or (string? mod) (symbol? mod))
     (build-path (output-directory) "modules" (~a mod ".js"))]
    [else (error "module names are either string or symbol")]))

