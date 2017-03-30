(module interop '#%kernel
  (#%provide #%js-ffi)
  ;; #%js-ffi is treated specially by compiler to interact with JS or do
  ;; operations which are not otherwise possible within Racket.

  ;;
  ;; #%js-ffi : Operation Any ... -> Any
  ;; WHERE:
  ;; - Operation is one of -
  ;;   + 'ref
  ;;   + 'index
  ;;   + 'var
  ;;   + 'assign
  ;;   + 'new
  ;;   + 'object
  ;;   + 'array
  ;;   + 'require
  (define-values (#%js-ffi)
    (lambda _
      (#%app error 'racketscript "can't make JS ffi calls in Racket"))))
