#lang scribble/manual

@title[#:style '(toc)]{The RacketScript Language and Compiler}

@defmodule[racketscript/base #:lang #:use-sources (racketscript/interop)]

@(author
  (author+email "Vishesh Yadav" "vishesh3y@gmail.com" #:obfuscate? #t)
  (author+email "Stephen Chang" "stchang@racket-lang.org" #:obfuscate? #t))

RacketScript is an experimental Racket to JavaScript (ES6)
compiler. It allows programmers to use both JavaScript's and Racket's
ecosystem and aims to make this interoperability as smooth as
possible.

@local-table-of-contents[]

@include-section{start.scrbl}
@include-section{ffi.scrbl}
