#lang racket/base

(require racket/match
         racket/string
         racket/syntax
         syntax/readerr)

(provide (rename-out [x-read read]
                     [x-read-syntax read-syntax]))

(define (x-read in)
  (syntax->datum (x-read-syntax #f in)))


(define (x-read-syntax src in)
  (skip-whitespace in)
  (read-rapture src in))


(define (skip-whitespace in)
  (regexp-match #px"^\\s*" in))


(define (read-rapture src in)
  (define-values (line col pos) (port-next-location in))
  (define *pattern* #px"([[:alnum:]$_]+)(\\.[[:alnum:]$_]+)*(\\[\"[[:alnum:]$_]+\"\\])*")

  (define (match-pattern offset)
    (match (regexp-match *pattern* in offset)
      [#f (raise-read-error "bad interop syntax"
                            src line col pos
                            (and pos (- (file-position in) pos)))]
      [expr-match expr-match]))

  (define (peek-char=? offset chr)
    (char=? (peek-char in offset) chr))

  (define (parse-literal s δ result first-id?)
    (match (or (regexp-match #px"^[[:alnum:]$_]+" s δ)
               (regexp-match #px"^\\.[[:alnum:]$_]+" s δ)
               (regexp-match #px"^\\[\"[[:alnum:]$_]+\"\\]" s δ))
      [`(,v) #:when (string-prefix? v ".")
       (define v-len (string-length v))
       (define id-sym (quasiquote
                       (quote
                        (unquote (string->symbol (substring v 1))))))
       (define stx
         (datum->syntax #f
                        `(#%plain-app #%js-ffi 'ref ,result ,id-sym)))
       (parse-literal s (+ v-len δ) stx #f)]
      [`(,v) #:when (string-prefix? v "[")
       (define v-len (string-length v))
       (define id-sym (quasiquote
                       (quote
                        (unquote (string->symbol (substring v 2 (- v-len 2)))))))
       (define stx
         (datum->syntax #f
                        `(#%plain-app #%js-ffi 'index ,result ,id-sym)))
       (parse-literal s (+ v-len δ) stx #f)]
      [`(,v)
       (define v-len (string-length v))
       (define id-sym (let ([s (string->symbol v)])
                        (if first-id?
                            (quasiquote (unquote s))
                            (quasiquote (quote (unquote s))))))
       (define stx `(#%plain-app #%js-ffi 'var ,id-sym))
       (parse-literal s (+ v-len δ) stx #f)]
      [#f result]))

  (cond
    [(and (peek-char=? 0 #\j)
          (peek-char=? 1 #\s)
          (peek-char=? 2 #\.))
     (define expr-match (match-pattern 3))
     (parse-literal (bytes->string/utf-8 (car expr-match)) 0 #f #t)]
    [(and (peek-char=? 0 #\j)
          (peek-char=? 1 #\s)
          (peek-char=? 2 #\*)
          (peek-char=? 3 #\.))
     (define expr-match (match-pattern 4))
     (parse-literal (bytes->string/utf-8 (car expr-match)) 0 #f #f)]
    [else #f]))
