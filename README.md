# Rapture

A lightweight Racket to JavaScript compiler

## Installation

Following system packages are required -

- Racket 6.4 or higher
- NodeJS and NPM

Then install following NodeJS and Racket modules -

    ## If you install locally, make sure they are in PATH
    $ npm install -g traceur js-beautify gulp

If you do not wish to pollute your root npm directory, you can set a
custom global location by changing your `npmrc` (eg. 
`echo "prefix = $HOME/.npm-packages" >> ~/.npmrc`

Then add `/prefix/path/above/bin` to your `PATH`.

    ## Racket dependencies
    $ raco pkg install threading

The compiler is written in Typed Racket. To avoid long startups, it is
advised to pre-compile Racket sources to bytecode.

    # From source root
    $ make build

## Basic Usage
    
    $ rapture -h
    
    rapture [ <option> ... ] <filename>
     where <option> is one of
      -d <dir>, --build-dir <dir> : Output directory
      -n, --skip-npm-install : Skip NPM install phase
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

    # Override default output directory
    $ rapture -d /path/to/output/dir foobar.rkt
    
This will produce `dist/compiled.js`. To execute inside NodeJS, execute
`bootstrap.js` in output directory. For running in browser, include the Traceur
runtime.

A more robust (and less portable) way, is to run the ES6 modules generated in
`modules` directly from Traceur. Goto `modules` output directory and execute
`$ traceur foobar.js`.
