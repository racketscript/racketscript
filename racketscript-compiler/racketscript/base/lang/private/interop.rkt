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
  (read-racketscript src in))


(define (skip-whitespace in)
  (regexp-match #px"^\\s*" in))


(define (read-racketscript src in)
  (define-values (line col pos) (port-next-location in))
  (define *id-pattern* "([[:alnum:]$_]|\\p{Ll}|\\p{Lu})+")
  (define *string-pattern* "(\\p{L}|\\p{N}|\\p{Pd})+")
  (define *pattern* (pregexp
                     (format "(~a)(\\.~a)*(\\[\"~a\"\\])*"
                             *id-pattern*
                             *id-pattern*
                             *string-pattern*)))

  (define (match-pattern offset)
    (match (regexp-match *pattern* in offset)
      [#f (raise-read-error "bad interop syntax"
                            src line col pos
                            (and pos (- (file-position in) pos)))]
      [expr-match expr-match]))

  (define (peek-char=? offset chr)
    (char=? (peek-char in offset) chr))

  (define (parse-literal s δ result first-id?)
    (match (or (regexp-match (pregexp (format "^~a" *id-pattern*)) s δ)
               (regexp-match (pregexp (format "^\\.~a" *id-pattern*)) s δ)
               (regexp-match (pregexp (format "^\\[\"~a\"\\]" *string-pattern*)) s δ))
      [`(,v ,_) #:when (string-prefix? v ".")
       (define v-len (string-length v))
       (define id-sym (quasiquote
                       (quote
                        (unquote (string->symbol (substring v 1))))))
       (define stx
         (datum->syntax #f
                        `(#%plain-app #%js-ffi 'ref ,result ,id-sym)))
       (parse-literal s (+ v-len δ) stx #f)]
      [`(,v ,_) #:when (string-prefix? v "[")
       (define v-len (string-length v))
       (define field (substring v 2 (- v-len 2)))
       (define stx
         (datum->syntax #f
                        `(#%plain-app #%js-ffi 'index ,result ,field)))
       (parse-literal s (+ v-len δ) stx #f)]
      [`(,v ,_)
       (define v-len (string-length v))
       (define stx
         (datum->syntax #f
                        (let ([s (string->symbol v)])
                          (if first-id?
                              s
                              `(#%plain-app #%js-ffi 'var ',s)))))
       (parse-literal s (+ v-len δ) stx #f)]
      [#f result]))

  (cond
    [(and #;(peek-char=? 0 #\j)
          (peek-char=? 0 #\s)
          (peek-char=? 1 #\.))
     (define expr-match (match-pattern 2))
     (parse-literal (bytes->string/utf-8 (car expr-match)) 0 #f #t)]
    [(and #;(peek-char=? 0 #\j)
          (peek-char=? 0 #\s)
          (peek-char=? 1 #\*)
          (peek-char=? 2 #\.))
     (define expr-match (match-pattern 3))
     (parse-literal (bytes->string/utf-8 (car expr-match)) 0 #f #f)]
    [else #f]))
