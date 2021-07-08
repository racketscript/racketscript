<img src="logo.svg" align="right" height="110" />

# RacketScript

[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](COPYING.md)
[![Tests](https://github.com/racketscript/racketscript/actions/workflows/racket.yml/badge.svg)](https://github.com/racketscript/racketscript/actions/workflows/racket.yml)
[![ESLint](https://github.com/racketscript/racketscript/actions/workflows/node.js.yml/badge.svg)](https://github.com/racketscript/racketscript/actions/workflows/node.js.yml)
[![Coverage Status](https://codecov.io/gh/racketscript/racketscript/coverage.svg?branch=master)](https://codecov.io/gh/racketscript/racketscript?branch=master)
[![Try Online](https://img.shields.io/badge/try_it-online!-ff9900.svg)](http://play.racketscript.org)

RacketScript is an **experimental** lightweight Racket to JavaScript (ECMAScript 6)
compiler. RacketScript aims to leverage both JavaScript and Racket's ecosystem,
and make interoperability between them clean and smooth.

RacketScript takes in Racket source files, uses Racket's macro expander to
produce [Fully Expanded
Programs](https://docs.racket-lang.org/reference/syntax-model.html#%28part._fully-expanded%29),
and then compile these fully expanded programs to JavaScript. RacketScript
currently supports only a subset of Racket.

## Try RacketScript

You can try RacketScript in your browser
at [RacketScript Playground](http://play.racketscript.org).

## Disclaimer

RacketScript is **work-in-progress** and is not mature and stable. Several
Racket features and libraries are not yet implemented (eg. number pyramid,
contracts, proper tail calls, continuations). There are also quite a few missing
primitive functions. That said, we encourage experimentation, user feedback,
discussions, bug reports and pull requests.

## Installation

Following system packages are required:

- [Racket](http://www.racket-lang.org/) 6.12 or higher
- [NodeJS](https://nodejs.org/) (14.0 or higher) and NPM
- Make

### Quick Install

RacketScript can be installed using the Racket package manager `raco`:

```sh
raco pkg install racketscript
```

See [Basic Usage](#basic-usage) to get started.

### Install from Github

```sh
# Clone RacketScript
git clone git@github.com:racketscript/racketscript.git`
cd racketscript

# Build and install
make setup
```

If you do not wish to pollute your root NPM directory, you can set a
custom global location by changing your `npmrc` (eg.  `echo "prefix =
$HOME/.npm-packages" >> ~/.npmrc`. Then add `/prefix/path/above/bin`
to your `PATH`.

## Basic Usage

RacketScript compiler is named `racks`. 

```sh
racks -h # show help
```

To compile a Racket source file:

```sh
# Installs all NPM dependencies and compile file.rkt
racks /path/to/file.rkt
```

The above command will create a output build directory named
`js-build`, copy RacketScript runtime, copy other support files,
install NPM dependencies, compile `file.rkt` and its dependencies.

The compiled JavaScript modules typically goto one of following three
folders:

- "modules": The normal Racket files.
- "collects": Racket collects source files.
- "links": Other third party packages.
- "dist": Contains sources compiled to ES6 or bundled JavaScript ready
  for distribution.

Here are few other examples that would come in handy:

```sh
# To skip `npm install` step. Useful when building
# for second time.
racks -n /path/to/module-name.rkt

# Run the assembled JavaScript module.
node js-build/modules/module-name.rkt.js

# Use `-b` to format the assembled JavaScript code use `-b`. Assumes
# `js-beautify` is available in `$PATH`.
racks -b /path/to/module-name.rkt

# Override default output directory
racks -d /path/to/output/dir /path/to/module-name.rkt

# Print JavaScript output to stdout
racks --js --js-beautify /path/to/module-name.rkt
```

By default tail call optimization is turned off. To enable translation
of self recursive tail calls to loop, pass `--enable-self-tail` flag.

```sh
racks --enable-self-tail /path/to/source.rkt
```

### Browser

Most browsers can load RacketScript modules directly without any external
dependencies `<script type="module" src="path/to/module.rkt.js"></script>`.

### Module Bundler (Webpack)

For deployment, you may want to bundle all generated modules into single
JavaScript file. RacketScript can generate some boiler-plate for using
Webpack/Babel, however we recommend you to use your own configuration.

```sh
# Use `--target` or `-t` flag.
racks --target webpack /path/to/source.rkt

# Call webpack to bundle in `js-build` directory. Will produce
# single JavaScript bundle in `js-build/dist` directory.
npx webpack
```

## Contributing to RacketScript

Please read [Contribution Guidelines](CONTRIBUTING.md).

## Troubleshooting

Please read the [Troubleshooting Wiki](https://github.com/racketscript/racketscript/wiki/Troubleshooting).

## Related Work

- [Whalesong](https://github.com/soegaard/whalesong)
- [Urlang](https://github.com/soegaard/urlang)
