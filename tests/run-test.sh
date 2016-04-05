#!/bin/sh

## Run all test cases and see if any one fails to executes. Doesn't check
## if the result is actually correct. Log files chould be checked manually
## to see what went wrong

TESTDIR="$1"
PATTERN="$2"
RAPTURE=`realpath ../bin/rapture`
NODEJS=node
TRACEUR=traceur

if [ -z "$TESTDIR" ]; then
    echo "usage: ./run-test.sh <testdir> [pattern]"
    exit 1;
fi

if [ -z "$PATTERN" ]; then
    PATTERN="*.rkt"
else
    PATTERN="$2.rkt"
fi

cd $TESTDIR

rm -rf ./logs/
mkdir -p ./logs/

OPTS=""

for f in `ls $PATTERN`; do
    echo "+-------------------------------+"
    TESTCASE=${f%.rkt}
    echo "TESTCASE: $TESTCASE\n"
    echo "$TESTCASE\n\n====== RACKET OUTPUT ====== \n" > ./logs/$TESTCASE.log
    racket $f >> ./logs/$TESTCASE.log 2>&1

    echo "\n\n======= RAPTURE OUTPUT ====== \n" >> ./logs/$TESTCASE.log
    $RAPTURE $OPTS -g $f >> ./logs/$TESTCASE.log 2>&1

    echo "\n\n======= EXECUTE OUTPUT ======= \n" >> ./logs/$TESTCASE.log
    cd js-build/modules
    $TRACEUR $TESTCASE.js >> ../../logs/$TESTCASE.log 2>&1
    RESULT=$?
    cd ../../

    if [ "$RESULT" -eq "0" ]; then
        echo "OK.\n"
        echo "PASSED   $TESTCASE" >> ./logs/summary.txt
    else
        echo "!!!!! FAILED !!!!!\n"
        echo "FAILED   $TESTCASE" >> ./logs/summary.txt
    fi

    OPTS="-n"
    # TODO: Check equality. But before that, output format should be same
done

