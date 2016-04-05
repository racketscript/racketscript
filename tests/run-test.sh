#!/bin/sh

## Run all test cases and see if any one fails to executes. Doesn't check
## if the result is actually correct. Log files chould be checked manually
## to see what went wrong

TESTDIR="$1"
RAPTURE=`realpath ../bin/rapture`
NODEJS=node
TRACEUR=traceur

if [ -z "$TESTDIR" ]; then
    echo "usage: ./run-test.sh <testdir>"
    exit 1;
fi

cd $TESTDIR
mkdir -p ./logs/

OPTS=""

for f in `ls *.rkt`; do
    echo "+-------------------------------+"
    TESTCASE=${f%.rkt}
    echo "TESTCASE: $TESTCASE\n"
    echo "$TESTCASE\n\n====== RACKET OUTPUT ====== \n" > ./logs/$TESTCASE.out
    racket $f >> ./logs/$TESTCASE.out 2>&1

    echo "\n\n======= RAPTURE OUTPUT ====== \n" >> ./logs/$TESTCASE.out
    $RAPTURE $OPTS $f >> ./logs/$TESTCASE.compile.out 2>&1

    echo "\n\n======= EXECUTE OUTPUT ======= \n" >> ./logs/$TESTCASE.out
    cd js-build/modules
    $TRACEUR $TESTCASE.js > ../../logs/$TESTCASE.rjs.out 2>&1

    if [ "$?" -eq "0" ]; then
        echo "OK. PASSED"
    else
        echo "!!!!! FAILED !!!!!"
    fi

    OPTS="-n"

    echo ""
    cd ../../

    # TODO: Check equality. But before that, output format should be same
done

