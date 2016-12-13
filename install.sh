#!/bin/sh

set -e

NODEJS_EXE=node
RACO_EXE=raco

echo "RacketScript Installation"
echo "  [https://github.com/vishesh/racketscript]"
echo "==========================================="
echo

if ! type "$NODEJS_EXE" > /dev/null; then
    echo "NodeJS (node) not found in \$PATH. [https://nodejs.org/]"
    exit 1
fi

if ! type "$RACO_EXE" > /dev/null; then
    echo "Racket not found in \$PATH [http://www.racket-lang.org/]"
    exit 1
fi

raco pkg install -t github vishesh/racketscript/?path=racketscript-compiler#master
raco pkg install -t github vishesh/racketscript/?path=racketscript-extras#master

echo "RacketScript installed successfully. Enjoy!"
