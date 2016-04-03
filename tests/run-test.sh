#!/bin/sh

TESTDIR="$1"
RAPTURE=`realpath ../bin/rapture`
NODEJS=node

if [ -z "$TESTDIR" ]; then
    echo "usage: ./run-test.sh <testdir>"
    exit 1;
fi

cd $TESTDIR

for f in `ls *.rkt`; do
    echo "==================================================================="
    echo "TESTCASE: $f"
    RACKET_OUTPUT=`racket $f`
    echo "RACKET OUTPUT : $RACKET_OUTPUT"

    mkdir -p ./builds/$f
    $RAPTURE -d ./builds/$f $f 2>&1 > ./builds/$f.compile.out
    RAPTURE_OUTPUT=`$NODEJS builds/$f/bootstrap.js 2> ./builds/$f.run.out`
    echo "RAPTURE OUTPUT: $RAPTURE_OUTPUT"

    # TODO: Check equality. But before that, output format should be same
done

