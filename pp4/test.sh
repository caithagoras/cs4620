#!/bin/bash

tests=(bad1 bad2 bad3 bad4 bad6 bad10 bad11 bad12 matrix new queue stack t1 t2 t3 t4 t5 t6 t7 t9 t10 t12 t13)

for test in ${tests[@]}
do
    ./dcc < "samples/$test.decaf" &> cpr
    diff -s cpr "samples/$test.out"
done

rm -f cpr

