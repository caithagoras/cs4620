#!/bin/bash

tests=(badlink badnewarr badsub factorial matrix queue stack t1 t2 t3 t4 t6 t7 t8)
#tests=(factorial)

for test in ${tests[@]}
do
    ./run "samples/$test.decaf" solution/dcc > "$test.ans"
    ./run "samples/$test.decaf" ./dcc > "$test.out"
    diff -sq "$test.ans" "$test.out"
    rm -f "$test.ans" "$test.out"
done

rm -f cpr out

