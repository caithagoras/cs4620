#!/bin/bash

file=$1;

./dcc < "samples/$file.frag" > cpr
diff -s cpr "samples/$file.out"
