#lang racketscript/boot

(require racketscript/interop
         "lib.rkt"
         "kernel.rkt")

(define Linklet ($/require/* "./core/racketscript.js"))
(define Core ($/require/* "./core.js"))

;; FIXME duplicate from kernel.rkt
(define equal? #js.Core.isEqual)

(define (object->hash o)
  (let ([entries (#js*.Object.entries o)])
    (($ entries 'reduce)
     (λ (table pair) (hash-set (string->symbol (js-string->string ($ pair 0))) ($ pair 1)))
     (hash))))

(define+provide (primitive-table table-name)
  (cond
    [(equal? table-name '#%kernel) #js.Core.KernelTable.kernelTable]
    [(equal? table-name '#%linklet)
     (let ([lnkTable (object->hash #js.Linklet)])
       (hash-set lnkTable 'primitive-table primitive-table))]
    [(equal? table-name '#%paramz) paramz-table] ;; imported from kernel.rkt
    ;; Pycket has entries for all of the tables below
    [(equal? table-name '#%foreign) (hash)]
    [(equal? table-name '#%unsafe)  (hash)]
    [(equal? table-name '#%futures) (hash)]
    [(equal? table-name '#%place)   (hash)]
    [(equal? table-name '#%flfxnum) (hash)]
    [(equal? table-name '#%extfl)   (hash)]
    [(equal? table-name '#%network) (hash)]


    [else #f]))

