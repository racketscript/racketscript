#lang racket/base

(require racketscript/ffi)


(define console ($ 'global 'console))

(define (fun x)
  (if (zero? x)
    ($/obj [name      "Vishesh"]
           [location  "Boston"])
    ($/obj [onfoo     (λ (n)
                        ($$ console.log "Called foo: " n)
                        (fun n))]
           [onbar     (λ (n)
                        ($$ console.log "Called bar " n)
                        (fun n))])))

($ ($> (fun 10)
       (onfoo 10)
       (onbar 20)
       (onfoo 30)
       (onbar 40)
       (onbar 50)
       (onfoo 0))
   'name)
