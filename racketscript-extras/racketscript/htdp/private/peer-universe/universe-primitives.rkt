#lang racketscript/base

(require ;htdp/error
         racket/list)

(provide sexp?
         
         make-package
         package?
         package-world
         package-message

         make-bundle
         bundle?
         
         make-mail
         mail?
         
         iworld-name
         iworld?
         iworld=?
         
         ;; private
         bundle-state
         bundle-mails
         bundle-low-to-remove

         ;; private
         mail-to
         mail-content

         ;; private
         make-iworld
         iworld-conn)

(define (sexp? x)
  (cond
    [(empty? x) #true]
    [(string? x) #true]
    [(bytes? x) #true]
    [(symbol? x) #true]
    [(number? x) #true]
    [(boolean? x) #true]
    [(char? x) #true]
    [(pair? x) (and (list? x) (andmap sexp? x))]
    ; [(and (struct? x) (prefab-struct-key x)) (for/and ((i (struct->vector x))) (sexp? i))]
    [else #false]))

(struct u-package (world message))
(define (make-package world message)
  (u-package world message))
(define (package? p)
  (u-package? p))
(define (package-world p)
  (u-package-world p))
(define (package-message p)
  (u-package-message p))

(struct u-bundle (state mails low-to-remove))
(define (make-bundle state mails low-to-remove)
  (u-bundle state mails low-to-remove))
(define (bundle? bundle)
  (u-bundle? bundle))
(define (bundle-state b)
  (u-bundle-state b))
(define (bundle-mails b)
  (u-bundle-mails b))
(define (bundle-low-to-remove b)
  (u-bundle-low-to-remove b))

(struct u-mail (to content))
(define (make-mail to content)
  (u-mail to content))
(define (mail? mail)
  (u-mail? mail))
(define (mail-to mail)
  (u-mail-to mail))
(define (mail-content mail)
  (u-mail-content mail))

(struct u-iworld (conn name))
;; for client code use
(define (iworld-name iworld)
  (u-iworld-name iworld))
(define (iworld? iworld)
  (u-iworld? iworld))
(define (iworld=? iw1 iw2)
  (define conn1 (u-iworld-conn iw1))
  (define conn2 (u-iworld-conn iw2))
  ($/binop === conn1 conn2))
;; not for client code use
(define (make-iworld conn name)
  (u-iworld conn name))
(define (iworld-conn iw)
  (u-iworld-conn iw))