#lang racket

(provide links-module?
         improper->proper
         *jsident-pattern*
         js-identifier?)

(require setup/link
         setup/dirs
         "logging.rkt")

;; Path Path -> Boolean
;; Returns true if path has base as prefix
(define (subpath? base path)
  (define base* (explode-path base))
  (define path* (explode-path path))
  (cond
    [(>= (length base*) (length path*)) #f]
    [else
     (for/and ([b base*]
               [p path*])
       (equal? b p))]))

;; Module-Path -> (Maybe (list String Path))
;; If `mod-path` belongs to a module listed in (find-links-file),
;; return a list containing:
;; - the link name,
;; - and path to root of the module
;; e.g., '("racketscript-compiler"
;;         #<path:/home/username/racketscript/racketscript-compiler>)
;; else return false.
(define (links-module? mod-path)
;  (printf "links-module? mod-path: ~v\n" mod-path)
  (define links-file (find-links-file))
  (for/or ([link-path (links #:file links-file #:root? #t)])
    ;; (printf "link: ~v\n" link)
    ;; (define link-name
    ;;   (~a (let-values ([(base last dir?) (split-path link-path)]) last)))
    ;; (printf "link-name: ~v\n" link-name)
    ;; (printf "link-path: ~v\n" link-path)
    (and (subpath? link-path mod-path)
#;         (printf "link result: ~v\n" 
                   (list link-name link-path))
         (let-values ([(base link-name dir?) (split-path link-path)])
           (list (~a link-name) link-path)))))

#;(define (links-module? mod-path)
  (printf "links-module? mod-path: ~v\n" mod-path)
  (define (match-link? dir link)
    (printf "match-link?: ~v\n" dir)
    (printf "match-link?: ~v\n" link)
    (match link
      [(list 'root path) #:when (absolute-path? path)
       ;; Links.rktd may have point to root package which is not at current
       ;; subdir. Eg. /usr/local/.../links.rktl may point to a package in
       ;; home directory.
       (subpath? (~a (simplify-path path))
                 (~a mod-path))]
      [(list name path)
       (printf "match-link? link name: ~v\n" name)
       (printf "match-link? link path: ~v\n" path)
       (subpath? (~a (simplify-path (build-path dir path)))
                 (~a mod-path))]
      [(list name path re) #f]))

  ;; Path LinkEntry -> (list Symbol Path)
  ;; Returns (list link-name pkg-root-dir)
  ;; WHERE: LinkEntry is  an entry in links.rktd file
  (define (link->result link-fpath link)
    (printf "link->result: ~a\n" link-fpath)
    (printf "link->result: ~a\n" link)
    ;; HACK: If the link path is relative path, then we pick
    ;; the last component
    (define (fix-relative-path p)
      (if (relative-path? p)
          (~a (let-values ([(base last dir?) (split-path p)]) last))
          p))
    (match link
      [(list 'root path) #:when (absolute-path? path)
       (list (~a (let-values ([(base last dir?) (split-path path)]) last))
             (string->path path))]
      [(list name path)
       (list (if (symbol? name)
                 (fix-relative-path path)
                 name)
             (simplify-path (build-path (path-only link-fpath)
                                        path)))]
      [_ (error 'link->result "unsupported form")]))

  ;; Path -> (list Symbol Path)
  ;; Iterate through each entry in links.rktd file pointed
  ;; by link-fpath and find the entry with module mod-path
  (define (find-link link-fpath)
    (log-rjs-debug "Processing library collection links at: ~a" link-fpath)
    (define links-dir (path-only link-fpath))
    (cond
      [(file-exists? link-fpath)
       (call-with-input-file link-fpath
         (λ (p-links-in)
           (let loop ([links (read p-links-in)])
             (printf "links: ~v\n" links)
             (match links
               ['() #f]
               [(cons hd tl) (if (match-link? links-dir hd)
                                 (link->result link-fpath hd)
                                 (loop tl))]))))]
      [else
       (log-rjs-warning "Library collection link file ~a does not exist!" link-fpath)
       #f]))

  ;; Iterate through each links.rktd file, to find
  ;; the out
  (let loop ([links (current-library-collection-links)])
    (match links
      ['() #f]
      [(cons #f tl) (loop tl)]
      [(cons hd tl) (or (find-link hd)
                        (loop tl))])))


(define (improper->proper l)
  (match l
    [(cons a b) (cons a (improper->proper b))]
    ['() '()]
    [_ (cons l '())]))
(module+ test
  (check-equal? (improper->proper '()) '())
  (check-equal? (improper->proper '(1 . 2)) '(1 2))
  (check-equal? (improper->proper '(1 2 3 . 4)) '(1 2 3 4))
  (check-equal? (improper->proper '(1 2 3)) '(1 2 3))
  (check-equal? (improper->proper '(1 2 (3 . 4) . 5)) '(1 2 (3 . 4) 5)))

(define *jsident-start-letter* "\\p{L}|\\p{Nl}|\\$|_")
(define *jsident-rest-letters* (string-append
                                "\\p{L}|\\p{Nl}|\\$|_|\\p{Mn}|\\p{Mc}|\\p{Nd}|\\p{Pc}"
                                "|\u200D|\u200C"))
(define *jsident-pattern* (format "(~a)(~a)*"
                                  *jsident-start-letter*
                                  *jsident-rest-letters*))

;; Symbol -> Boolean
;; Returns true if `sym` is a valid JavaScript identifier
(define (js-identifier? sym)
  (define str (symbol->string sym))
  (regexp-match-exact? (pregexp *jsident-pattern*) str))

(module+ test
  (require rackunit))
