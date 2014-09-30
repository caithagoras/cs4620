#!/bin/bash

tests=(bad1 bad2 bad3 bad4 bad5 bad6 bad7 class control decl simple expressions functions interface inheritance matrix incrdecr switch)

for test in ${tests[@]}
do
    ./dcc < "samples/$test.decaf" &> cpr
    diff -s cpr "samples/$test.out"
done

rm -f cpr

