# Rapture

A lightweight Racket to JavaScript compiler

## Installation

Following system packages are required -

- Racket 6.4 or higher
- NodeJS and NPM

Then install following NodeJS and Racket modules -

    ## If you install locally, make sure they are in PATH
    $ npm install -g traceur js-beautify

    ## Racket dependencies
    $ raco pkg install threading

The compiler is written in Typed Racket. To avoid long startups,
its advised to do pre-compile Racket sources to bytecode.

    # From source root
    $ make build

## Basic Usage

    
    $ rapture -h
    rapture [ <option> ... ] <filename>
     where <option> is one of
      -d <dir>, --build-dir <dir> : Output directory
    / --ast : Expand and print AST
    | --il : Compile to intermediate langauge (IL)
    \ --js : Compile to JS
      --help, -h : Show this help
      -- : Do not treat any remaining argument as a switch (at this level)
     /|\ Brackets indicate mutually exclusive options.
     Multiple single-letter switches can be combined after one `-'; for
      example: `-h-' is the same as `-h --'


Default output directory is named `js-build`. To compile a Racket file
named `foobar.rkt`, run -

    $ rapture foobar.rkt
    
This will produce bunch of files in output directory. Currently, you can
run `bootstrap.js` in NodeJS.
