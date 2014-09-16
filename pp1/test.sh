#!/bin/bash

frags=(badbool baddouble badident badint badop badreserve badstring comment ident number reserve_op string)
programs=(program2 program3)

for file in ${frags[@]}
do
    ./dcc < "samples/$file.frag" &> cpr
    diff -s cpr "samples/$file.out"
done

for file in ${programs[@]}
do
    ./dcc < "samples/$file.decaf" &> cpr
    diff -s cpr "samples/$file.out"
done

rm -f cpr
