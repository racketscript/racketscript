#lang s-exp syntax/module-reader
racketscript/boot

#:read x-read
#:read-syntax x-read-syntax

(require (prefix-in interop: "private/interop.rkt"))
(provide x-read x-read-syntax)

;; Do or don't do renaming of fieldname
;; Do or don't do renaming of identifier names
;; Do or don't choose whether to index or subscript
(define (make-x-readtable)
  (make-readtable (current-readtable)
                  #\j 'dispatch-macro
                  read-x))

(define (x-read in)
  (parameterize ([current-readtable (make-x-readtable)])
    (read in)))

(define (x-read-syntax in stx)
  (parameterize ([current-readtable (make-x-readtable)])
    (read-syntax in stx)))

(define read-x
  (case-lambda
    [(ch in)
     (interop:read in)]
    [(ch in src lin col pos)
     (interop:read-syntax src in)]))
