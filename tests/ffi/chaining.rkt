#lang racketscript/base

(require racketscript/interop)

(define (fun x)
  (if (zero? x)
    ($/obj [name      #js"Vishesh"]
           [location  #js"Boston"])
    ($/obj [onfoo     (λ (n)
                        (#js*.console.log #js"Called foo: " n)
                        (fun n))]
           [onbar     (λ (n)
                        (#js*.console.log #js"Called bar: " n)
                        (fun n))])))
(#js*.console.log
 ($ ($> (fun 10)
        (onfoo 10)
        (onbar 20)
        (onfoo 30)
        (onbar 40)
        (onbar 50)
        (onfoo 0))
    'name))
