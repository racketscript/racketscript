#lang racket

(provide links-module?
         improper->proper)

(define (links-module? mod-path)
  (define (match-link? dir link)
    (match link
      [(list 'root path) #f]
      [(list name path)
       (string-prefix? (~a mod-path)
                       (~a (simplify-path (build-path dir path))))]
      [(list name path re) #f]))
  
  ;; Returns (list link-name pkg-root-dir)
  (define (link->result link-fpath link)
    (match link
      [(list name path) #:when (and (string? name)
                                    (path-string? path))
       (list name
             (simplify-path (build-path (path-only link-fpath)
                                        path)))]
      [_ (error 'link->result "unsupported form")]))
  
  (define (find-link link-fpath)
    (define links-dir (path-only link-fpath))
    (let loop ([links  (read (open-input-file link-fpath))])
      (match links
        ['() #f]
        [(cons hd tl) (if (match-link? links-dir hd)
                          (link->result link-fpath hd)
                          (loop tl))])))

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
