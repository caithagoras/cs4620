#!/bin/bash

tests=(badlink factorial t1 t6)
#tests=(factorial)

for test in ${tests[@]}
do
    ./run "samples/$test.decaf" solution/dcc > "$test.ans"
    ./run "samples/$test.decaf" ./dcc > "$test.out"
    diff -sq "$test.ans" "$test.out"
    rm -f "$test.ans" "$test.out"
done

rm -f cpr out

