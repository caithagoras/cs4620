#!/bin/bash

tests=(bad1 bad2 bad3 bad4 bad5 bad6 bad7 bad8 bad9 blackjack matrix t1 t2 t3 t4 t5 t6 t7 t8 t9 t10)

for test in ${tests[@]}
do
    ./dcc < "../pp3/samples/$test.decaf" &> cpr
    diff -s cpr "../pp3/samples/$test.out"
done

rm -f cpr

