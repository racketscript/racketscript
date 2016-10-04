#lang racket/base

(require "config.rkt"
         (for-syntax racket/base))

(provide log-rjs-info)

(define-syntax (log-rjs-info stx)
  (syntax-case stx ()
    [(_ a0 a* ...)
     #'(when (logging?)
         (begin (printf "[info]")
                (unless (equal? (string-ref a0 0) #\[)
                  (printf " "))
                (printf a0 a* ...)
                (printf "\n")))]))

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
