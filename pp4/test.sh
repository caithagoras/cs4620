#!/bin/bash

tests=(bad1 bad2 bad10)

for test in ${tests[@]}
do
    ./dcc < "samples/$test.decaf" &> cpr
    diff -s cpr "samples/$test.out"
done

rm -f cpr

