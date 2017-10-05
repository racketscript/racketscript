#lang racketscript/base

(require racketscript/interop)

(define obj {$/obj [name        "John"]
                   [city        "San Jose"]
                   [occupation  "Driver"]})

(for ([(k v) (in-js-object obj)])
  (displayln (list k v)))

(displayln 
  (for/hash ([(k v) (in-js-object obj)])
            (values k v)))
