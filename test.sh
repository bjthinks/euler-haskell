#!/bin/sh

problem_numbers=`ls -1 P*.hs | tr -d 'P.hs' | sort -n`
test_numbers=`awk '{print $1}' testdata | sort -n`
if [ "$problem_numbers" != "$test_numbers" ]
then
    echo "Missing test data:"
    echo $problem_numbers | tr ' ' '
'> zzz1
    echo $test_numbers | tr ' ' '
'> zzz2
    diff zzz1 zzz2
    rm zzz1 zzz2
    exit 1
fi

for line in `sort -n testdata | tr ' ' '_'`
do
    line=`echo $line | tr '_' ' '`
    problem=`echo $line | awk '{print $1}'`
    echo "Testing problem $problem..."
    expected_answer=`echo $line | awk '{print $2}'`
    ghc --make -O3 -XFlexibleContexts P$problem
    actual_answer=`./P$problem`
    if [ "x$expected_answer" != "x$actual_answer" ]
    then
	echo "Error in problem $problem: expected $expected_answer, got $actual_answer"
	exit 1
    fi
done
