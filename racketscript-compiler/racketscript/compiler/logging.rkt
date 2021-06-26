#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         (for-meta 2 syntax/parse)
         "config.rkt")

(provide log-rjs-info
         log-rjs-debug
         log-rjs-warning
         log-rjs-error)

(define-syntax log-rjs
  (syntax-parser
    [(_ kind:id)
     #:with name (format-id #'kind "log-rjs-~a" (syntax-e #'kind) #:source #'kind)
     #:with str (symbol->string (syntax-e #'kind))
     (syntax (define-syntax name
               (syntax-parser
                 [(_ a0 a* (... ...))
                  #'(when (logging?)
                      (begin (printf "[~a]" 'str)
                             (unless (equal? (string-ref a0 0) #\[)
                               (printf " "))
                             (printf a0 a* (... ...))
                             (printf "\n")))])))]))
(log-rjs info)
(log-rjs error)
(log-rjs warning)
(log-rjs debug)

;; (define-logger rjs)
;; (define rjs-info (make-log-receiver rjs-logger 'info))
;; (define rjs-dbg  (make-log-receiver rjs-logger 'debug))

;; (void
;;  (thread
;;   (λ()
;;     (let loop ()
;;       (define v (sync rjs-info rjs-dbg))
;;       (printf "[~a] ~a\n" (vector-ref v 0) (vector-ref v 1))
;;       (loop)))))
