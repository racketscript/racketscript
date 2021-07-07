#lang scribble/manual

@(require scribble/example
          racketscript/interop
          (for-label racket/base
                     racketscript/interop))

@title{RacketScript-JavaScript FFI}

RacketScript supports full interoperability with JavaScript. This section explains how JavaScript code is invoked in a RacketScript program.

@section[#:tag "raw"]{The Main FFI Form}

All JavaScript FFI calls begin with @racket[#%js-ffi], followed by a
symbol indicating the JavaScript grammar production to be generated.

@defform*[((#%js-ffi type . rest))
          #:grammar
          ([type symbol?])]{
All JavaScript FFI calls begin with @racket[#%js-ffi], followed by a
symbol indicating the JavaScript grammar production to be generated.
                                    }

@section[#:tag "reader"]{Reader Extensions}

RacketScript includes a reader extension that makes it easier to make
certain JavaScript calls. Specifically, RacketScript's reader
recognizes @racket{#js} and @racket{#js*} delimiters, which enable
access to variables in the JavaScript namespace.
