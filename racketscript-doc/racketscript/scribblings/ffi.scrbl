#lang scribble/manual

@(require (only-in scribble-enhanced
                   [defform enhanced:defform]
                   [defform* enhanced:defform*])
          (for-label racket/base
                     racket/contract/base
                     racketscript/interop))

@title[#:tag "rs-js-ffi"]{The RacketScript-JavaScript FFI}

@defmodule[racketscript/interop #:use-sources (racketscript/interop)]

RacketScript supports near-complete interoperability with all
JavaScript features. This section explains how to invoke plain
JavaScript operations in a RacketScript program.

@section[#:tag "js-ffi"]{RacketScript's JavaScript FFI Primitive}


@enhanced:defform*[((#%js-ffi 'var)
           (#%js-ffi 'ref obj prop-id)
           (#%js-ffi 'index obj prop-expr)
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
           (#%js-ffi 'string str)
           (#%js-ffi 'require mod)
           (#%js-ffi 'operator 'op operand ...))
]{

@bold{NOTE}: Users most likely @bold{should not} be using this form. Instead,
use the API in @secref{mainapi}, which will expand to the
appropriate call to @racket[#%js-ffi].

The @racket[#%js-ffi] form in RacketScript compiles diretly to various
JavaScript features. A symbol follows the initial @racket[#%js-ffi],
indicating the kind of JavaScript code to be generated, followed by
any arguments.}

Summary of JavaScript operations supported by @racket[#%js-ffi]:
@itemlist[@item{@racket['var]: Use to access variable in the JavaScript namespace}
          @item{@racket['ref]: JavaScript object property reference, i.e., dot notation}
          @item{@racket['index]: JavaScript index operation, i.e., bracket notation}
          @item{@racket['assign]: JavaScript assignment}
          @item{@racket['new]: JavaScript object constructor}
          @item{@racket['throw]: Throw JavaScript exception}
          @item{@racket['undefined]: JS @tt{undefined} value}
          @item{@racket['null]: JS @tt{null} object value}
          @item{@racket['this]: JS @tt{this} object self reference}
          @item{@racket['arguments]: implicit JS @tt{arguments} variable containing function args}
          @item{@racket['object]: JS object literals, i.e, curly brace notation}
          @item{@racket['array]: JS array literals, i.e, bracket notation}
          @item{@racket['typeof]: JS @tt{typeof} operation}
          @item{@racket['instanceof]: JS @tt{instanceof} operation}
          @item{@racket['string]: JS strings (incompatible with Racket/RacketScript strings, see @racket[$/str])}
          @item{@racket['require]: JS @tt{import}, use to import JS libraries}
          @item{@racket['operator]: Use to call JS functions requiring infix notation}
          ]

@section[#:tag "mainapi"]{RacketScript's JavaScript FFI API}

@defform*[(($ jsid)
           ($ expr sym)
           ($ expr expr)
           ($ expr expr ...))
          #:grammar
          ([jsid (code:line valid JS identifier (alphanumeric underscore and dollar chars))])
          #:contracts
           ([sym symbol?])]{

Syntax for accessing Javascript variables, and referencing and indexing properties.

 @itemlist[@item{A single identifier argument corresponds to a JavaScript variable.

                 @bold{Note}: the identifier be a @bold{valid JavaScript identifier} (underscore, dollar, numbers, and letters only), and not Racket or RacketScript.

           Equivalent to @racket[(#%js-ffi 'var jsid)].}

           @item{Supplying a second argument that is a symbol correponds to a JavaScript object reference, i.e., dot notation, where the second argument is the property name.

                 @bold{Example}: When handling a web request @racket[req], getting the body of a request could be written @racket[($ req 'body)] which compiles to @tt{req.body} in JavaScript.

                 Equivalent to @racket[(#%js-ffi 'ref req 'body)].

                 @bold{Note}: The above assumes that @racket[req] is a RacketScript variable. If the variable is in the JavaScript namespace only, then an additional @racket[$] is needed, e.g., @racket[($ ($ window) 'document)], which is equivalent to @racket[(#%js-ffi 'ref (#%js-ffi 'var window) 'body)], compiles to @tt{window.document} in JavaScript.}
           
           @item{A second argument that is an arbitrary expression is treated as JavaScript bracket index notation, e.g., @racket[($ req "body")] compiles to @tt{req["body"]} in JavaScript.


                 Equivalent to @racket[(#%js-ffi 'index req "body")].}
           @item{Supplying multiple arguments is compiled to a series of bracket index lookups}]

@defform[($$ dot-chain e ...)
         #:grammar
         ([dot-chain (code:line symbol or identifier consisting of multiple dot-separated names)])]{
Shorthand for multiple @racket[$]s. Allows more direct use of dot notation in RacketScript. E.g., @racket[($$ window.document write)]}                                  
                                                    }

@defform[($/new constructor-expr)]{JavaScript object construction. Equivalent to @racket[(#%js-ffi 'new constructor-expr)].}
@defform[($/throw exn)]{Throw a JavaScript exception. Equivalent to @racket[(#%js-ffi 'throw exn)].}
@defform[#:id $/undefined $/undefined]{The JavaScript @tt{undefined} value. Equivalent to @racket[(#%js-ffi 'undefined)]}
@defform[#:id $/null $/null]{The JavaScript @tt{null} object. Equivalent to @racket[(#%js-ffi 'null)].}
@defform[#:id $/this $/this]{The JavaScript @tt{this} keyword. Equivalent to @racket[(#%js-ffi 'this)].}
@defform[#:id $/arguments $/arguments]{The JavaScript @tt{arguments} object containing the arguments passed to a function. Equivalent to @racket[(#%js-ffi 'arguments)].}

@defform[($/obj [fld v] ...)
         #:grammar ([fld identifier])]{JavaScript object literal notation, i.e., brace notation, where @tt{fld} are identifiers representing the object's properties, and @tt{v ...} are values assigned to those properties. Equivalent to @racket[(#%js-ffi 'object fld ... v ...)]}

@defform[($/:= e v)]{JavaScript assignment statement. Equivalent to @racket[(#%js-ffi 'assign e v)]. @racket[e] should be a ref, index, or symbol}

@defform[($/array e ...)]{JavaScript array literal notation, where @racket[($/array 1 2 3)] is equivalent to @tt{[1,2,3]}. Equivalent to @racket[(#%js-ffi 'array e ...)]}

@defform*[#:literals (*)
          (($/require mod)
           ($/require mod *))
          #:contracts
          ([mod string?])]{
   JavaScript import statement.
                                         
   Often used with @racket[define], e.g., @racket[(define express ($/require "express"))] compiles to:

   @tt{import * as express from "express";}
   
    Equivalent to @racket[(#%js-ffi 'require mod)] or  @racket[(#%js-ffi 'require '* mod)]}


@defform[($/require/* mod)
         #:contracts
         ([mod string?])]{
    JavaScript import all statement.

    Shorthand for @racket[($/require mod *)]}

@defform[($> e call ...)
         #:grammar
         ([call id
                (meth arg ...)])]{
    JavaScript chaincall.

    For example:

    @tt{($> (#js.res.status 400) (send #js"Bad Request"))}

    is compiles to @tt{res.status(400).send("Bad Request")}
    
    Equivalent to nested @racket[#%js-ffi] calls (with @racket['var], @racket['ref], or @racket['index]).
}

@defform*[(($/typeof e)
           ($/typeof e type))
          #:contracts
          ([type (and/c string?
                        (or/c "undefined" "object" "boolean" "number" "string" "function"))])]{
JavaScript @tt{typeof} operator.
                                                                                               
The first form returns a string representing the typeof the given JavaScript value. Equivalent to @racket[(#%js-ffi 'typeof e)].

The second form is shorthand for checking the type of a value. For example, @racket[($/typeof 11 "number")] is compiles to

@tt{typeof 11 === "number";}

Equivalent to @racket[($/binop === (#%js-ffi 'typeof e) ($/str v))]
}

@defform[($/instanceof e type) #:grammar ([e (code:line JavaScript Object)])]{Returns a boolean indicating whether JavaScript object @racket[e] is an instance of @racket[type].

                  Equivalent to @racket[(#%js-ffi 'instanceof e type)]}


@defform[($/binop op operand1 operand2)]{JavaScript infix binary function call.

Equivalent to @racket[(#%js-ffi 'operator 'op operand1 operand2)]}

@defform[($/+ operand ...)]{Multi-argument infix calls to JavaScript @tt{+} (can be used as either concat or addition).
                  Equivalent to multiple nested calls to @racket[$/binop]}

@defproc[(js-string->string [jsstr JSstring]) string?]{Converts a JS string to a RacketScript string.}

@defproc[(js-string [str string?]) JSstring]{Converts a RacketScript string to a JS string.}

@defform[($/str s)]{Converts a Racket string to a JS string, or vice versa, using @racket[js-string->string] or @racket[js-string].}

@section[#:tag "reader"]{Reader Extensions}

Using @tt{#lang racketscript/base} includes reader extensions that makes it easier to make
certain JavaScript calls. Specifically, RacketScript's reader
recognizes three delimiters:

@itemlist[@item{@verbatim|{#js}|

                Used to make a series of references using dot notation.

                @bold{Example}: @verbatim|{#js.req.body}| where @racket[req] is a RacketScript variable.

                Equivalent to a series of @racket[#%js-ffi] @racket['ref] calls.}

          @item{@verbatim|{#js*}|

                Used to make a series of references using dot notation. The difference with @racket{#js} is that @racket{#js*} wraps the first identifier in a @racket[#%js-ffi] @racket['var] form, i.e., it is used to call methods on JavaScript variables instead of RacketScript variables.
                                        

                @bold{Example}: @verbatim|{#js*.JSON.parse}| where @racket[JSON] is a JavaScript variable.

                Equivalent to a series of @racket[#%js-ffi] @racket['ref] calls where the first id is wrapped in a @racket[#%js-ffi] @racket['var].}

          @item{@verbatim|{#js"js string"}|

                Used to create JS strings.

                @bold{Note}: JS strings are not compatible with Racket/RacketScript strings. Use @racket[$/str] and other related API functions to convert between the two when needed.

                @bold{Example}: @verbatim|{(#js*.console.warn #js"Error!")}|

                Equivalent to a @racket[#%js-ffi] call with @racket['string].}]
