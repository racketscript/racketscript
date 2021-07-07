#lang scribble/manual

@(require (for-label racket/base))

@title[#:style '(toc)]{The @racketmodname[racketscript] Language and Compiler}

@defmodule[racketscript #:lang #:use-sources (racketscript)]

@(author
  (author+email "Vishesh Yadav" "vishesh3y@gmail.com" #:obfuscate? #t)
  (author+email "Stephen Chang" "stchang@racket-lang.org" #:obfuscate? #t))

RacketScript is an experimental lightweight Racket to JavaScript (ES6)
compiler. It allows programmers to use both JavaScript's and
Racket's ecosystem, and makes interoperability between them clean and
smooth.

@local-table-of-contents[]

@include-section{start.scrbl}
@include-section{ffi.scrbl}
