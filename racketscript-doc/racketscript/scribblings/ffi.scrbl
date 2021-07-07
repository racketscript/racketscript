#lang scribble/manual

@(require (for-label racket/base
                     racketscript/interop))

@title[#:tag "rs-js-ffi"]{The RacketScript-JavaScript FFI}

RacketScript supports full interoperability with JavaScript. This section explains how JavaScript code is invoked in a RacketScript program.

@section[#:tag "js-ffi"]{The Main FFI Form}


@defform*[((#%js-ffi 'ref . rest)
           (#%js-ffi 'index . rest)
           (#%js-ffi 'var . rest)
           (#%js-ffi 'assign . rest)
           (#%js-ffi 'new . rest)
           (#%js-ffi 'object . rest)
           (#%js-ffi 'array . rest)
           (#%js-ffi 'this)
           (#%js-ffi 'arguments)
           (#%js-ffi 'require . rest))
          #:grammar
          ([type symbol?])]{

@bold{Note}: Users most likely should not be using this form. Instead, use the appropriate forms in the subsequent sections.
                            
All JavaScript FFI calls begin with @racket[#%js-ffi], followed by a
symbol indicating the JavaScript grammar production to be generated.
                                    }

@section[#:tag "main"]{Main FFI Forms}

@defform[($/this)]{Equivalent to @racket[(#%js-ffi 'this)]}
@defform[($/arguments)]{Equivalent to @racket[(#%js-ffi 'arguments)]}
@defform[($/null)]{Equivalent to @racket[(#%js-ffi 'null)]}

@section[#:tag "reader"]{Reader Extensions}

RacketScript includes a reader extension that makes it easier to make
certain JavaScript calls. Specifically, RacketScript's reader
recognizes @racket{#js} and @racket{#js*} delimiters, which enable
access to variables in the JavaScript namespace.
