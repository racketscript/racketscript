#lang racket/base

;; take some of the forms from the anaphoric library, without including it as a dependency
;; for the project.
;;
;; Taken or modified from: https://github.com/SuzanneSoy/anaphoric

(require racket/stxparam
         (for-syntax racket/base))

(provide it acond)

(define-syntax-parameter it
  (λ (stx) 
    (raise-syntax-error
      'it
      "Use of the \"it\" identifier is only allowed within anaphoric macros."
      stx)))

(define-syntax (acond stx)
  (syntax-case stx (else)
    [(_ [else . else-body]) #'(begin . else-body)]
    [(_) #'(void)]
    [(_ [condition . body] . rest)
     #'(let ([tmp condition])
         (if tmp
           (syntax-parameterize ([it (make-rename-transformer #'tmp)])
             . body)
           (acond . rest)))]))
