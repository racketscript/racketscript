#lang scribble/manual

@(require (for-label racket/base
                     racketscript/interop))

@title[#:tag "rs-js-ffi"]{The RacketScript-JavaScript FFI}

@defmodule[racketscript/interop]

RacketScript supports full interoperability with JavaScript. This section explains how to invoke plain JavaScript calls in a RacketScript program.

@section[#:tag "js-ffi"]{The Main FFI Form}


@defform*[#:id ref
          ((#%js-ffi 'ref obj prop-id)
           (#%js-ffi 'index obj prop-expr)
           (#%js-ffi 'var)
           (#%js-ffi 'assign x e)
           (#%js-ffi 'new expr)
           (#%js-ffi 'throw exn)
           (#%js-ffi 'undefined)
           (#%js-ffi 'null)
           (#%js-ffi 'this)
           (#%js-ffi 'arguments)
           (#%js-ffi 'object [fld v] ...)
           (#%js-ffi 'array args ...)
           (#%js-ffi 'typeof obj)
           (#%js-ffi 'instanceof obj type)
           (#%js-ffi 'require mod)
           (#%js-ffi 'operator 'op operand ...)
           )]{

All JavaScript FFI calls begin with @racket[#%js-ffi], followed by a
symbol indicating the kind of JavaScript code to be generated, followed by any arguments.

@bold{Note}: Users most likely should not be using this form. Instead, use the forms the subsequent sections, which will expand to the appropriate call to @racket[#%js-ffi].
                          
                                    }

@section[#:tag "main"]{Main FFI Forms}

@defform*[(($ jsid)
           ($ expr sym)
           ($ expr expr)
           ($ expr expr ...))
          #:grammar
          ([jsid (code:line valid JS idenfitier)]
           [sym symbol?])]{
 Syntax for accessing Javascript variables directly.

 @itemlist[@item{A single identifier argument corresponds to a JavaScript variable. Note that is must be a valid JavaScript identifier (underscore, dollar, numbers, and letters only)}
           @item{Supplying a second argument that is a symbol correponds to JavaScript object reference dot notation, where the second argument is the property name. E.g., getting the body of a request could be written @racket[($ req 'body)] which compiles JS @tt{req.body}}
           @item{A second argument that is an arbitrary expression is treated as JavaScript bracket index notation, e.g., @racket[($ req "body")] is compiled to JS @tt{res["body"]}}
           @item{Supplying multiple arguments is compiled to a chain of bracket index lookups}]

@defform[($$ dotted-id e ...)
         #:grammar
         ([dotted-id (code:line symbol or identifier using dot notation)])]{
Shorthand for multiple @racket[$]s. Allows using some form of dot notation. E.g., @racket[($$ window.document write)]}                                  
                                                    }

@defform[($/new expr)]{JavaScript object constructor. Equivalent to @racket[(#%js-ffi 'new expr)]}
@defform[($/throw exn)]{Throw a JavaScript exception. Equivalent to @racket[(#%js-ffi 'throw exn)]}
@defform[#:id $/undefined $/undefined]{The JavaScript @tt{undefined} value. Equivalent to @racket[(#%js-ffi 'undefined)]}
@defform[#:id $/null $/null]{The JavaScript @tt{null} object. Equivalent to @racket[(#%js-ffi 'null)]}
@defform[#:id $/this $/this]{The JavaScript @tt{this} keyword. Equivalent to @racket[(#%js-ffi 'this)]}
@defform[#:id $/arguments $/arguments]{The JavaScript @tt{arguments} object containing the arguments passed to a function. Equivalent to @racket[(#%js-ffi 'arguments)]}

@defform[($/obj [fld v] ...)
         #:grammar ([fld identifier])]{JavaScript object literal notation, where @tt{fld} are identifiers representing the object's properties, and @tt{v ...} are values assigned to those properties. Equivalent to @racket[(#%js-ffi 'object fld ... v ...)]}

@defform[($/:= e v)]{JavaScript assignment statement. Equivalent to @racket[(#%js-ffi 'assign e v)]. @racket[e] should be a ref, index, or symbol}

@defform[($/array e ...)]{JavaScript array literal notation, where @racket[($/array 1 2 3)] is equivalent to @tt{[1,2,3]}. Equivalent to @racket[(#%js-ffi 'array e ...)]}

@defform*[#:literals (*)
          (($/require mod)
           ($/require mod *))
          #:grammar
          ([mod string?])]{
   JavaScript import statement.
                                         
   Typically used with @racket[define], e.g., @racket[(define express ($/require "express"))] is equivalent to

   @tt{import * as express from "express";}
   
    Equivalent to @racket[(#%js-ffi 'require mod)] or  @racket[(#%js-ffi 'require '* mod)]}


@defform[($/require/* [mod string?])]{
    JavaScript import all statement.

    Shorthand for @racket[($/require mod *)]}

@defform[($> e call ...)
         #:grammar
         ([call id
                (meth arg ...)])]{
    JavaScript chaincall.

    For example:

    @tt{($> (#js.res.status 400) (send #js"Bad Request"))}

    is equivalent to @tt{res.status(400).send("Bad Request")}
    
    Equivalent to nested @racket[#%js-ffi] calls (with @racket['var], @racket['ref], or @racket['index])
}

@defform*[(($/typeof e)
           ($/typeof e type))
          #:grammar
          ([type (and/c string?
                        (or/c "undefined" "object" "boolean" "number" "string" "function"))])]{
JavaScript @tt{typeof} operator.
                                                                                               
The first form returns a string representing the typeof the given JavaScript value. Equivalent to @racket[(#%js-ffi 'typeof e)].

The second form is shorthand for checking the type of a value. For example, @racket[($/typeof 11 "number")] is equivalent to

@tt{typeof 11 === "number";}

Equivalent to @racket[($/binop === (#%js-ffi 'typeof e) ($/str v))]
}

@defform[($/instanceof e type) #:grammar ([e (code:line JavaScript Object)])]{Returns a boolean indicating whether JavaScript object @racket[e] is an instance of @racket[type].

                  Equivalent to @racket[(#%js-ffi 'instanceof e type)]}


@defform[($/binop op operand1 operand2)]{JavaScript infix binary function call.Equivalent to @racket[(#%js-ffi 'operator 'op operand1 operand2)]}

@defform[($/+ operand ...)]{Multi-argument infix calls to JavaScript @tt{+}
                  Equivalent to multiple nested calls to @racket[$/binop]}

@defform[($/str s)]{Converts a Racket string to a JS string, or vice versa.}

@section[#:tag "reader"]{Reader Extensions}

RacketScript includes a reader extension that makes it easier to make
certain JavaScript calls. Specifically, RacketScript's reader
recognizes @racket{#js} and @racket{#js*} delimiters, which enable
access to variables in the JavaScript namespace.
