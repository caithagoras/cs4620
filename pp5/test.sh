#!/bin/bash

anstests=(badlink badnewarr badsub factorial matrix queue stack t1 t2 t3 t4 t6 t7 t8)
gentests=(badlink badnewarr badsub blackjack factorial fib life matrix minesweep peoplesearch queue sort stack t1 t2 t3 t4 t5 t6 t7 t8 tictactoe)

printf "Test Run Results:\n"
for test in ${anstests[@]}
do
    ./run "samples/$test.decaf" solution/dcc > "$test.ans"
    ./run "samples/$test.decaf" ./dcc > "$test.out"
    diff -sq "$test.ans" "$test.out"
    rm -f "$test.ans" "$test.out"
done

printf "\nTest Generated Code:\n"

for test in ${gentests[@]}
do
    solution/dcc < "samples/$test.decaf" > "$test.ans" 2>&1
    ./dcc < "samples/$test.decaf" > "$test.out" 2>&1
    diff -sq "$test.ans" "$test.out"
    rm -f "$test.ans" "$test.out"
done

rm -f cpr out

