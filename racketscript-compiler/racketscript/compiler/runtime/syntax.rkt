#lang racketscript/boot

(require racketscript/interop
         racket/stxparam
         (for-syntax syntax/parse)
         "lib.rkt")

(provide (rename-out [-syntax? syntax?]
                     [-datum->syntax datum->syntax]
                     (-syntax->datum syntax->datum)
                     [-syntax-e syntax-e]
                     (-syntax-source syntax-source)
                     (-syntax-line syntax-line)
                     (-syntax-column syntax-column)
                     (-syntax-position syntax-position)
                     (-syntax-span syntax-span)
                     (-syntax-property syntax-property)
                     (-syntax-property-symbol-keys syntax-property-symbol-keys)))

(define (-syntax? v) (#js.Core.Correlated.syntax_p v))
(define (-datum->syntax v) (#js.Core.Correlated.datum_to_syntax v))
(define (-syntax-e v) (#js.v.get))
(define (-syntax->datum v) (#js.v.get))
(define (-syntax-source v) #f)
(define (-syntax-line v) #f)
(define (-syntax-column v) #f)
(define (-syntax-position v) #f)
(define (-syntax-span v) #f)
(define (-syntax-property s k [val #f])
  (if val s #f))

(define (-syntax-property-symbol-keys v) #js.Core.Pair.EMPTY)
