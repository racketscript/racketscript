#lang racket/base

(require racket/match
         racket/string
         racket/syntax
         syntax/readerr
         (only-in racketscript/compiler/util-untyped
                  js-identifier?))

(provide (rename-out [x-read read]
                     [x-read-syntax read-syntax]))

(define (x-read in)
  (syntax->datum (x-read-syntax #f in)))

(define (x-read-syntax src in)
  (skip-whitespace in)
  (read-racketscript src in))

(define (skip-whitespace in)
  (regexp-match #px"^\\s*" in))

(define (parts->ffi strs first-id?)
  (define result (cond
                   [first-id? (car strs)]
                   [(js-identifier? (car strs))
                    `(#%js-ffi 'var ',(car strs))]
                   [else (error 'ffi "invalid characters in js identifier")]))
  (let loop ([strs (cdr strs)]
             [result result])
    (match strs
      ['() (datum->syntax #f result)]
      [(cons hd tl)
       (loop tl
             `(#%js-ffi 'ref ,result ',hd))])))

(define (get-id-parts in)
  (map string->symbol (string-split
                       (symbol->string
                        (syntax-e
                         (read-syntax (object-name in) in)))
                       ".")))

(define (read-racketscript src in)
  (define-values (line col pos) (port-next-location in))

  (define (peek-char=? offset chr)
    (char=? (peek-char in offset) chr))

  (cond
    [(and #;(peek-char=? 0 #\j)
          (peek-char=? 0 #\s)
          (peek-char=? 1 #\.)
          (read-string 2 in))
     (parts->ffi (get-id-parts in) #t)]
    [(and #;(peek-char=? 0 #\j)
          (peek-char=? 0 #\s)
          (peek-char=? 1 #\*)
          (peek-char=? 2 #\.)
          (read-string 2 in))
     (parts->ffi (get-id-parts in) #f)]
    [(and (peek-char=? 0 #\s)
          (peek-char=? 1 #\")
          (read-string 1 in))
     (datum->syntax #f `(#%js-ffi 'string ,(read-syntax (object-name in) in)))]
    [else #f]))

(module+ test
  (require rackunit
           racketscript/interop)

  (define-simple-check (check-reader str first-id? result)
    (equal? (syntax->datum
             (read-racketscript #f (open-input-string
                                    (string-append
                                     (if first-id? "s." "s*.")
                                     str))))
            result))

  (check-reader "window" #t 'window)
  (check-reader "window" #f '(#%js-ffi 'var 'window))

  (check-reader "window.document" #t `(#%js-ffi 'ref window 'document))
  (check-reader "window.document.write" #t
                `(#%js-ffi 'ref (#%js-ffi 'ref window 'document) 'write))

  (check-reader "window.document" #f
                `(#%js-ffi 'ref (#%js-ffi 'var 'window) 'document))
  (check-reader "window.document.write" #f
                `(#%js-ffi 'ref
                           (#%js-ffi 'ref
                                     (#%js-ffi 'var 'window)
                                     'document)
                           'write)))
