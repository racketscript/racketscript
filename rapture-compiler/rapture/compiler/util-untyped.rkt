#lang racket

(provide links-module?
         improper->proper)


;; Path-String Path-String -> Boolean
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

;; Module-Path -> (Maybe (list Symbol Path))
;; Is mod-path belongs to a module listed in links file. If yes
;; return the link name in links.rktd file and path to root of
;; of that links module.
(define (links-module? mod-path)
  (define (match-link? dir link)
    (match link
      [(list name path)
       (subpath? (~a (simplify-path (build-path dir path)))
                 (~a mod-path))]
      [(list name path re) #f]))
  
  ;; Path LinkEntry -> (list Symbol Path)
  ;; Returns (list link-name pkg-root-dir)
  ;; WHERE: LinkEntry is  an entry in links.rktd file
  (define (link->result link-fpath link)
    (match link
      [(list name path)
       (list (if (symbol? name)
                 path
                 name)
             (simplify-path (build-path (path-only link-fpath)
                                        path)))]
      [_ (error 'link->result "unsupported form")]))
  
  ;; Path -> (list Symbol Path)
  ;; Iterate through each entry in links.rktd file pointed
  ;; by link-fpath and find the entry with module mod-path
  (define (find-link link-fpath)
    (define links-dir (path-only link-fpath))
    (let loop ([links  (read (open-input-file link-fpath))])
      (match links
        ['() #f]
        [(cons hd tl) (if (match-link? links-dir hd)
                          (link->result link-fpath hd)
                          (loop tl))])))

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

(module+ test
  (require rackunit))
