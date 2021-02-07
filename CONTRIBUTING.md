Contributing to RacketScript
============================

First off, thanks for taking the time to contribute! We need
volunteers to help this project grow and become successful.

The following is a set of guidelines for contributing to
RacketScript. These are mostly guidelines, not rules. Use your best
judgment, and feel free to propose changes to this document in a pull
request.

[Code of Conduct](#code-of-conduct)

[Reporting Bugs, Features and Enhancements](#reporting-bugs-features-and-enhancements)

[Hacking](#hacking)
- [Setup Development Environment](#setup-development-environment)
- [Testing](#testing)
- [Coverage](#coverage)

[Submitting Changes](#submitting-changes)

[Style Guide](#style-guide)

[Additional Resources](#additional-resources)

[Tips and Tricks](#tips-and-tricks)

## Code of Conduct

This project and everyone participating in it is governed by the
[RacketScript Code of Conduct](CODE_OF_CONDUCT.md). By participating,
you are expected to uphold this code. Please report unacceptable
behavior to [vishesh3y@gmail.com](mailto:vishesh3y@gmail.com).

## Reporting Bugs, Features and Enhancements

We use [Github Issues](https://github.com/vishesh/racketscript/issues)
for tracking bugs, features, enhancements and other
discussions.

- **Ensure the issue was not already reported** by searching on GitHub
  under [Issues](https://github.com/vishesh/racketscript/issues).
- If you're unable to find an open issue addressing the same, [open a
  new one](https://github.com/vishesh/racketscript/issues/new)
- Be sure to include a **title and clear description**, as much
  relevant information as possible, and a **code sample** or an
  **executable test case** demonstrating the expected behavior that is
  not occurring.

## Hacking

### Setup Development Environment

To start hacking on the RacketScript codebase you will need some extra
tools and packages for running tests, lints and coverage.

```sh
make setup-extra  # From RacketScript codebase root
```

The compiler is written in Typed Racket. To avoid long startup times, it is
advised to pre-compile Racket sources to bytecode.

```sh
make build  # From RacketScript codebase root
```

*Get familiar* with the various command line options provided by the
RacketScript CLI `racks` and Node.

### Testing

We have unit tests for testing compiler code, and integration tests
which tests both compiler and runtime.

#### Unit Tests

Unit tests cover the compiler codepath. Unit tests can be found in the
original Racket source files in the `test` submodule.

```sh
# Run unit tests
make unit-test
```

#### Integration Tests

Integration tests are used to do end-to-end testing of
RacketScript. Therefore, any compiler or runtime changes must pass the
integration test suite.

```sh
# Run integration tests
make integration-test
```

```
# Run all tests
make test
```

Integration tests can be found in the `tests/` subdirectory. The tests are
Racket code fragments which are compiled with both Racket and RacketScript.
The resulting Racket and JavaScript programs are then executed and a test fails
if the two runs do not produce identical outputs.

The tests are organized in various folders. Running the complete
integration test suite can take several minutes. `fixtures.rkt` can
run individual or a specific set of tests.

```sh
# To run specific test
$ ./fixture.rkt basic/let.rkt

# To run all tests in a folder
$ ./fixture.rkt basic/

# To run run all tests which match a glob pattern
$ ./fixture.rkt basic/procedure-*.rkt

# Its a good idea to get familiar with `fixture.rkt` as well
$ ./fixture.rkt -h
```

### Coverage

To produce a coverage report of the test suite:

```sh
# Consider coverage of all tests
$ make coverage

# Just consider unit-test coverage
$ make coverage-unit-test
```

Currently only the compiler codebase is covered in the report. The runtime,
including `kernel.rkt`, is *not* considered in this report even though
it actually is covered by our test suite.

## Submitting Patches

Make sure that there is an open ticket for the issue you want to work
on, to avoid duplicate effort and to keep the maintainers informed.

For small/trivial changes, you can submit a pull request
directly. In this case, make sure that the PR contains all
the information that you would otherwise provide in the issue ticket.

- Open a new GitHub pull request against `master` branch.
- Ensure the PR description clearly describes the problem and
  solution. Include the relevant issue number if applicable.
- Make sure the test suite passes and your changes are covered. Note
  that for runtime changes it is not possible to get coverage report
  (see [Coverage](#coverage)). We use
  [Travis](https://travis-ci.org/vishesh/racketscript) for continuous
  integration.
- Adhere to the [style guide](#style-guide) (you can use linters for
  JavaScript).

We like small patches. They are easier to reason about and review.

- Use your best judgment to split the pull request into a logical set of
  patches that tackles one problem at a time.
- Avoid making changes that are unnecessary for the current task at
  hand. They should ideally be a separate issue. If necessary, follow
  the previous point.
- Make sure your patches have a good commit message, title, and
  description, and reference the relevant Gitub issue number
  (see [additional resources on Git](#git)).

For larger changes, it is good practice to maintain open communication
in order to receive continuous feedback. This encourages participation from
the entire community and saves effort by avoiding major design changes at
later stages of the task.

## Style Guide

- [Racket Style Guide](http://docs.racket-lang.org/style/)
- [JavaScript Style Guide](https://github.com/airbnb/javascript)

## Tips and Tricks

#### Using multiple Racket versions

If you develop with multiple versions of Racket and don't have the
current Racket bin directory in your path, you may find the following
`racks` shim useful (place it in your bin path):

```bash
#!/bin/bash

"$(racket -e "(require setup/dirs) (display (find-user-console-bin-dir))")/racks" "$@"
```

#### Changing root NPM directory

If you do not wish to pollute your root NPM directory, you can set a
custom global location by changing your `npmrc` (eg.  `echo "prefix =
$HOME/.npm-packages" >> ~/.npmrc`. Then add `/prefix/path/above/bin`
to your `PATH`.


#### Get familiar with the tools

Get familiar with the tools used for development such as `racks`, `nodejs`,
`babel`, `make`, `fixture.rkt` etc. See the various targets available in
`Makefile` for usage examples.

Over the life of RacketScript, we've added various command line flags
to assist development workflow and to save time and misery. Don't hesitate
to file an issue with new ideas to improve this area!

## Additional Resources

#### Git, patches and pull requests

- [A note about Git commit message](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html)
- [Distributed Git - Contributing to a Project](https://git-scm.com/book/en/v2/Distributed-Git-Contributing-to-a-Project#Commit-Guidelines)
- [How to write the perfect pull request](https://github.com/blog/1943-how-to-write-the-perfect-pull-request)
- [The (written) unwritten guide to pull requests](https://www.atlassian.com/blog/git/written-unwritten-guide-pull-requests)

### Miscellaneous

- [Open Source Contribution Etiquette](http://tirania.org/blog/archive/2010/Dec-31.html)

## Attribution

These guidelines are adapted from:

- [Atom](https://github.com/atom/atom/blob/master/CONTRIBUTING.md)
- [Rails](https://github.com/rails/rails/blob/master/CONTRIBUTING.md)
- [Puppet](https://github.com/puppetlabs/puppet/blob/master/CONTRIBUTING.md)
